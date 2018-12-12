module.exports = {
  config: {
    paths: {
      watched: ["app"]
    },
    files: {
      javascripts: {
        joinTo: "js/app.js"
      },
      stylesheets: {
        joinTo: "css/app.css"
      }
    },
    npm: {
      styles: {
        'tachyons': [
          'css/tachyons.min.css',
        ],
      },
    },
    plugins: {
      elmBrunch: {
        mainModules: ["app/elm/Main.elm"],
        makeParameters: ['--debug'],
        outputFolder: "public/js/"
      },
      sass: {
        mode: "native"
      }
    }
  }
};