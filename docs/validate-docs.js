const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');
const { parseTransaction } = require('libcardano/cardano/serialization');

const projectRoot = path.resolve(__dirname, './'); // Path to the directory containing package.json
const docsDir = path.join(__dirname, 'docs'); // Corrected path to point to docs/docs
const tempDir = path.join(__dirname, 'temp-validation');

function ensureDir(dir) {
  if (!fs.existsSync(dir)) {
    fs.mkdirSync(dir, { recursive: true });
  }
}

function createTsConfig(dir) {
  const tsconfigContent = JSON.stringify({
    compilerOptions: {
      // allowJs: true, // Removed to ensure strict TypeScript checking
      // checkJs: true, // Removed to ensure strict TypeScript checking
      noEmit: true,
      esModuleInterop: true,
      forceConsistentCasingInFileNames: true,
      strict: true,
      // skipLibCheck: true, // Removed to ensure module resolution is strict
      lib: ["es2020", "dom", "esnext.asynciterable"], // Added 'esnext.asynciterable'
      module: "commonjs", // Ensure CommonJS modules are understood
      target: "es2020",
      baseUrl: "..", // Set baseUrl to parent directory to resolve modules from project root node_modules
      paths: {
        "libcardano": ["./node_modules/libcardano"],
        "kuber-client": ["./node_modules/kuber-client"]
      }
    },
    include: ["./**/*"]
  }, null, 2);
  fs.writeFileSync(path.join(dir, 'tsconfig.json'), tsconfigContent);
}

function findMarkdownFiles(dir) {
  let markdownFiles = [];
  const files = fs.readdirSync(dir);
  for (const file of files) {
    const filePath = path.join(dir, file);
    const stat = fs.statSync(filePath);
    if (stat.isDirectory()) {
      markdownFiles = markdownFiles.concat(findMarkdownFiles(filePath));
    } else if (stat.isFile() && filePath.endsWith('.md')) {
      markdownFiles.push(filePath);
    }
  }
  return markdownFiles;
}

function extractCodeBlocksWithContext(filePath) {
  const content = fs.readFileSync(filePath, 'utf8');
  const lines = content.split('\n');
  const codeBlocks = [];
  let currentHeading = '';
  let inCodeBlock = false;
  let codeBlockLang = '';
  let codeBlockContent = [];
  let codeBlockStartLine = 0;

  // Regex to match the start of a TypeScript or JavaScript code block
  const codeBlockStartRegex = /^```(typescript|javascript)\s*$/;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    if (line.startsWith('## ')) {
      currentHeading = line.substring(3).trim();
    } else if (line.startsWith('### ')) {
      currentHeading = line.substring(4).trim();
    }

    const codeBlockStartMatch = line.match(codeBlockStartRegex);
    if (codeBlockStartMatch && !inCodeBlock) { // Start of a code block
      inCodeBlock = true;
      codeBlockLang = codeBlockStartMatch[1];
      codeBlockContent = [];
      codeBlockStartLine = i + 1;
    } else if (line.startsWith('```') && inCodeBlock) { // End of a code block
      inCodeBlock = false;
      codeBlocks.push({
        lang: codeBlockLang,
        code: codeBlockContent.join('\n'),
        filePath: filePath,
        line: codeBlockStartLine,
        heading: currentHeading
      });
      codeBlockContent = [];
      codeBlockLang = '';
    } else if (inCodeBlock) {
      codeBlockContent.push(line);
    }
  }
  return codeBlocks;
}

function validateCodeBlocks() {
  console.log('Starting documentation code example validation...');
  // Clean and ensure temp directory exists
  if (fs.existsSync(tempDir)) {
    fs.rmSync(tempDir, { recursive: true, force: true });
  }
  ensureDir(tempDir);
  createTsConfig(tempDir); // Create tsconfig.json in the temporary directory

  console.log(`Searching for markdown files in: ${docsDir}`);
  const markdownFiles = findMarkdownFiles(docsDir);

  let snippetCount = 0;
  const snippetMap = new Map(); // Map snippet file path to original markdown file and line

  for (const mdFile of markdownFiles) {
    const codeBlocks = extractCodeBlocksWithContext(mdFile);
    codeBlocks.forEach((block, index) => {
      // Only exclude code blocks under "Function Signature" heading in hydra-js-client docs
      const isHydraJsClientFile = mdFile.startsWith(path.join(docsDir, 'hydra-js-client'));
      const isFunctionSignature = block.heading === 'Function Signature';

      if (isHydraJsClientFile && isFunctionSignature) {
        // console.log(`Excluding code snippet from validation: ${path.relative(docsDir, block.filePath)} (line ${block.line}) under heading "${block.heading}"`);
        return; // Skip this block
      }

      const originalFileName = path.basename(block.filePath, '.md');
      const tempFileName = `${originalFileName}_${index}.${block.lang === 'typescript' ? 'ts' : 'js'}`;
      const tempFilePath = path.join(tempDir, tempFileName);
      
      // Wrap the code snippet to make it a valid module/script for TypeScript
      const wrappedCode = `
// Original file: ${block.filePath} (line ${block.line})
${block.code}
`;
      try {
        fs.writeFileSync(tempFilePath, wrappedCode);
        snippetMap.set(tempFilePath, { originalFile: block.filePath, originalLine: block.line });
        // console.log(`Mapped snippet: ${tempFilePath} -> ${block.filePath}:${block.line}`); // Debug log
        snippetCount++; // Increment snippetCount here
      } catch (writeError) {
        console.error(`Error writing snippet from ${block.filePath} (line ${block.line}):`, writeError.message);
      }
    });
  }

  if (snippetCount === 0) {
    console.log('No TypeScript or JavaScript code snippets found in markdown files.');
    return;
  }

  let staticTypeErrorsFound = false;
  try {
    const tscCommand = `npx tsc --noEmit --project ${tempDir} --pretty false`;
    console.log(`Executing static type validation command: ${tscCommand}`);
    const tscOutput = execSync(tscCommand, { encoding: 'utf8' });
    console.log('TypeScript Compiler Output (stdout):');
    console.log(tscOutput); // Print any non-error output from tsc
    console.log('All code snippets passed static type validation!');
  } catch (error) {
    staticTypeErrorsFound = true;
    console.error('TypeScript static type validation failed:');
    const tscErrorOutput = error.stdout ? error.stdout.toString() : error.message;
    // console.error('\n--- Raw TypeScript Compiler Error Output ---');
    // console.error(tscErrorOutput);
    // console.error('--------------------------------------------\n');
    
    // Parse tsc output to map errors back to original markdown files
    const errorLines = tscErrorOutput.split('\n');
    errorLines.forEach(line => {
      const match = line.match(/^(.*?)\((\d+),(\d+)\): error (TS\d+): (.*)$/);
      if (match) {
        const [, filePathInTemp, lineInTemp, charInTemp, errorCode, errorMessage] = match;
        const resolvedTempFilePath = path.resolve(filePathInTemp);
        // console.log(`Attempting to map error for: ${resolvedTempFilePath}`); // Debug log
        const originalInfo = snippetMap.get(resolvedTempFilePath); 
        if (originalInfo) {
          const adjustedLine = parseInt(lineInTemp) - 2; 
          const relativePath = path.relative(projectRoot, originalInfo.originalFile);
          console.error(`${relativePath}:${originalInfo.originalLine + adjustedLine} - error ${errorCode}: ${errorMessage}`);
        } else {
          console.error(`Could not map error for: '${resolvedTempFilePath}'`); // More specific fallback
          console.error('Available snippetMap keys:');
          snippetMap.forEach((value, key) => {
            console.error(`  '${key}'`);
          });
          console.error(`Original tsc error line: ${line}`);
        }
      } else {
        console.error(line); // Print lines that don't match the expected error format
      }
    });
  }

  if (staticTypeErrorsFound) {
    throw new Error('Documentation code validation failed due to static type errors.');
  }
}

validateCodeBlocks();


parseTransaction
