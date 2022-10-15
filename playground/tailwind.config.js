module.exports = {
  content: [
    "./index.html",
    "'./node_modules/tw-elements/dist/js/**/*.js'",
    "./src/**/*.{vue,js,ts,jsx,tsx}",
  ],
  theme: {
    extend: {
      width: {
        "1/20": "5%",
        "13/20": "65%",
        "6/20": "30%",
        "19/20": "95%",
      },
      height: {
        "1/16": "6.25%",
        "15/16": "93.75%",
        "1/12": "8.333333333%",
        "11/12": "91.666666667%",
        "1/15": "6.666666667%",
        "14/15": "93.333333333%",
        "1/14": "7.142857143%",
        "4/14": "28.571428571%",
        "5/14": "35.714285714%",
        "8/14": "57.142857143%",
        "9/14": "64.285714286%",
        "13/14": "92.857142857%",
      },
      colors: {
        primary: "#1D51DD",
        borderColor: "#D8D6D6",
        bgMenu: "#EFF2F7",
        menuBar: "#4E6FC6",
        bgCompiler: "#F5F5F5",
        bgFileTabBar: "#FBFBFB",
        fileTextColor: "#7C74A6",
        bgUtilities: "#FCFCFC",
        bgSelectedUtility: "#E1E1E1",
      },
    },
  },
  plugins: [require("tw-elements/dist/plugin")],
};
