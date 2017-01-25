'use strict';

// Require index.html so it gets copied to dist
require('./index.html');

require('./static/styles/style.css');
require('./static/styles/layout.css');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

var programFlags = {
  accessToken: sessionStorage.getItem ('accessToken')
};

// The third value on embed are the initial values for incomming ports into Elm
var app = Elm.Main.embed(mountNode, programFlags);

app.ports.storeAccessToken.subscribe (function (accessToken) {
  sessionStorage.setItem ('accessToken', accessToken);
});

// TODO: Figure out how popup that only gets the authorization code
// so we can post it to our backend and continue oauth from there

// window.fbAsyncInit = function() {
//   FB.init({
//     appId      : '1583083701926004',
//     version    : 'v2.7',
//     cookie     : true
//   });

//   app.ports.goToFacebookLoginCmd.subscribe(function() {
//     FB.login(function() {
//       console.log(arguments[0], arguments);
//     });
//   });

//   var status = FB.getLoginStatus();
//   console.log(status);
// };

// (function(d, s, id){
//    var js, fjs = d.getElementsByTagName(s)[0];
//    if (d.getElementById(id)) {return;}
//    js = d.createElement(s); js.id = id;
//    js.src = "//connect.facebook.net/en_US/sdk.js";
//    fjs.parentNode.insertBefore(js, fjs);
// }(document, 'script', 'facebook-jssdk'));
