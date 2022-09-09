module.exports = {
    content: [
      "./index.html",
      "'./node_modules/tw-elements/dist/js/**/*.js'",
      "./src/**/*.{vue,js,ts,jsx,tsx}",
    ],
    theme: {
      extend: {},
    },
    plugins: [
      require('tw-elements/dist/plugin')
  ]
  }