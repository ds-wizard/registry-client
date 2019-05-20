'use strict'

var program = require('./elm/Main.elm')

function getApiUrl() {
    if (window.registry && window.registry['apiUrl']) return window.registry['apiUrl']
    return 'http://localhost:3000'
}

var app = program.Elm.Main.init({
    flags: {
        apiUrl: getApiUrl(),
        credentials: JSON.parse(localStorage.getItem('credentials'))
    }
})

app.ports.saveCredentials.subscribe(function (credentials) {
    localStorage.setItem('credentials', JSON.stringify(credentials))
})
