!function e(t,n,o){function i(s,a){if(!n[s]){if(!t[s]){var h="function"==typeof require&&require;if(!a&&h)return h(s,!0);if(r)return r(s,!0);throw new Error("Cannot find module '"+s+"'")}var c=n[s]={exports:{}};t[s][0].call(c.exports,function(e){var n=t[s][1][e];return i(n?n:e)},c,c.exports,e,t,n,o)}return n[s].exports}for(var r="function"==typeof require&&require,s=0;s<o.length;s++)i(o[s]);return i}({1:[function(e,t){var n=function(e){for(var t,n,o=1,i=arguments.length;i>o;o++){t=arguments[o];for(n in t)t.hasOwnProperty(n)&&(e[n]=t[n])}return e};t.exports=n},{}],2:[function(e,t){var n=e("./toDashed"),o=e("./ie8/forEach"),i=function(e,t){var i,r;if(!e)throw new Error("Missing elm");return t=t||[],i="data-",r={},o(t,function(t){var o,s;o=i+n(t),s=e.getAttribute(o),/^(true|false)$/.test(s)&&(s="true"===s),/^\d+$/.test(s)&&(s=parseInt(s,10)),null!==s&&(r[t]=s)}),r};t.exports=i},{"./ie8/forEach":5,"./toDashed":8}],3:[function(e,t){var n=function(e,t,n){var o;for(o in e)e.hasOwnProperty(o)&&t.call(n,o,e[o])};t.exports=n},{}],4:[function(e,t){var n=function(e,t){return function(){return e.apply(t,arguments)}};t.exports=n},{}],5:[function(e,t){var n=function(e,t,n){var o,i;for(i=0,o=e.length;o>i;i++)t.call(n,e[i])};t.exports=n},{}],6:[function(e,t){var n=function(e,t,n){var o="on"+t;window.addEventListener?e.removeEventListener(t,n,!1):e.detachEvent(o,n)};t.exports=n},{}],7:[function(e,t){var n=function(e,t,n){var o="on"+t;window.addEventListener?e.addEventListener(t,n,!1):e.attachEvent(o,n)};t.exports=n},{}],8:[function(e,t){var n=function(e){var t;return e=e||"",t=/([A-Z])/g,e.replace(/([A-Z])/g,function(e){return"-"+e.toLowerCase()})};t.exports=n},{}],9:[function(e,t){var n=function(e){var t;return e=e||"",t=/([A-Z])/g,e.replace(/([A-Z])/g,function(e){return"_"+e.toLowerCase()})};t.exports=n},{}],10:[function(e,t){var n={onload:1,onunload:1,onblur:1,onchange:1,onfocus:1,onreset:1,onselect:1,onsubmit:1,onabort:1,onkeydown:1,onkeypress:1,onkeyup:1,onclick:1,ondblclick:1,onmousedown:1,onmousemove:1,onmouseout:1,onmouseover:1,onmouseup:1},o=function(e){var t,o;t=document,document.createEvent?(o=document.createEvent("Events"),o.initEvent(e,!0,!0)):document.createEventObject&&(o=document.createEventObject(),o.eventType=e),o.eventName=e,t.dispatchEvent?t.dispatchEvent(o):t.fireEvent&&n["on"+e]?t.fireEvent("on"+o.eventType,o):t[e]?t[e]():t["on"+e]&&t["on"+e]()};t.exports=o},{}],11:[function(e){function t(){if(this.KEYS=["returnTo","role","theme","token","locale","brandId","authOrigin","authDomain","showMobileDeeplink","mobileDeeplinkParams","action"],this.elm=document.currentScript||document.querySelector('[src*="/auth/v2"]'),!this.elm)throw new Error("Could not find script tag for zendesk_auth");if(this.declarativeOptions=r(this.elm,this.KEYS),!this.declarativeOptions.authDomain){var e;e=this.elm.getAttribute("src")||"",this.declarativeOptions.authDomain=e.replace(/\/auth\/v2.*/,"")}this.open=function(e){var t,n;return e=e||{},n||(t=i({},this.declarativeOptions,e),n=new o(t),n.onDestroyed(function(){n=void 0})),{close:function(){n.close()}}},this.declarativeOptions.action&&this.open()}var n,o=e("./lib/Host"),i=e("./helpers/extend"),r=e("./helpers/fetchDeclarativeAttrs");n=new t,window.Zendesk=window.Zendesk||{},window.Zendesk.Auth=function(e){return n.open(e)}},{"./helpers/extend":1,"./helpers/fetchDeclarativeAttrs":2,"./lib/Host":14}],12:[function(e,t){var n=e("../helpers/forIn"),o=function(){this.element=document.createElement("div"),this.styles={"-webkit-overflow-scrolling":"touch",overflow:"auto",position:"absolute",top:0,right:0,left:0,"z-index":99999},this.style()};o.prototype={style:function(){var e="";n(this.styles,function(t,n){e+=[t,":",n,"!important;"].join("")}),this.element.setAttribute("style",e)},changeStyles:function(e){e=e||{},n(e,function(e,t){this.styles[e]=t},this),this.style()},destroy:function(){this.element.parentNode&&this.element.parentNode.removeChild(this.element),this.element=null}},t.exports=o},{"../helpers/forIn":3}],13:[function(e,t){var n=function(e){"string"==typeof e&&(e=this.deserialize(e)),e=e||{},this.type=e.type||"",this.data=e.data||{}};n.prototype={serialize:function(){return JSON.stringify(this)},deserialize:function(e){return JSON.parse(e)}},t.exports=n},{}],14:[function(e,t){var n=e("./Iframe"),o=e("./Receiver"),i=e("../helpers/extend"),r=e("../helpers/triggerEvent"),s=e("../helpers/toUnderscore"),a=e("../helpers/ie8/forEach"),h=function(e){e=e||{},this.validParams=["role","returnTo","theme","token","locale","brandId","authOrigin","showMobileDeeplink","mobileDeeplinkParams"],this.options=i({action:"signin",authDomain:"",returnTo:window.location.href},e),this.load()};h.prototype={load:function(){this.frame||(this.frame=new n([this.options.authDomain,"/auth/v2/login/",this.options.action,"?",this.getParams()].join("")),this.receiver=new o(window,{acceptFromSource:this.frame.getWindow()}),this.receiver.on("auth:load_url",this.onLoadUrl,this),this.receiver.on("auth:goto_return_to",this.gotoReturnTo,this),this.receiver.on("auth:loaded",this.onLoaded,this),this.receiver.on("auth:close",this.close,this),this.receiver.on("auth:resize",this.onResize,this),r("auth:load"))},close:function(){r("auth:close"),this.destroy.apply(this,arguments)},getParams:function(){var e=[],t=decodeURIComponent(this.options.returnTo),n=t.match(/\/agent|oauth\//);return a(this.validParams,function(t){if(this.options.hasOwnProperty(t)){var n=s(t);e.push(n+"="+encodeURIComponent(this.options[t]))}},this),n&&e.push("role=agent"),e.join("&")},destroy:function(){this.frame.destroy(),this.receiver.destroy(),this.frame=void 0,this.receiver=void 0,this.onDestroyed.call(this)},onDestroyed:function(){},onResize:function(e){var t=e.height,n=window.innerHeight||document.documentElement.clientHeight;t>0&&this.frame.changeStyles({height:t+"px","min-height":n+"px"})},onLoadUrl:function(e){window.location.href=e.url},gotoReturnTo:function(){window.location.href=this.options.returnTo},onLoaded:function(){r("auth:loaded"),document.querySelector("body").scrollTop=1}},t.exports=h},{"../helpers/extend":1,"../helpers/ie8/forEach":5,"../helpers/toUnderscore":9,"../helpers/triggerEvent":10,"./Iframe":15,"./Receiver":17}],15:[function(e,t){var n=e("./Container"),o=e("../helpers/extend"),i=e("../helpers/forIn"),r=function(e,t){if(!e)throw new Error("src not provided");this.settings=o({src:e,autoLoad:!0,attachTo:"body"},t),this.element=document.createElement("iframe"),this.element.setAttribute("scrolling","no"),this.element.setAttribute("allowTransparency",!0),this.element.setAttribute("border",0),this.element.setAttribute("frameborder",0),this.container=(new n).element,this.styles={"z-index":"99999",display:"block","background-color":"transparent",border:"none",overflow:"hidden",visibility:"visible",margin:"0",padding:"0","-webkit-tap-highlight-color":"transparent",width:"100%",height:"100%"},this.settings.autoLoad&&this.load()};r.prototype={style:function(){var e="";i(this.styles,function(t,n){e+=[t,":",n,"!important;"].join("")}),this.element.setAttribute("style",e)},changeStyles:function(e){e=e||{},i(e,function(e,t){this.styles[e]=t},this),this.style()},attach:function(){var e=document.querySelector(this.settings.attachTo);e&&(this.container.appendChild(this.element),e.appendChild(this.container))},src:function(){this.element.src=this.settings.src},load:function(){this.style(),this.attach(),this.src()},getWindow:function(){return this.element.contentWindow},destroy:function(){this.container.parentNode&&this.container.parentNode.removeChild(this.container),this.container=null,this.element=null}},t.exports=r},{"../helpers/extend":1,"../helpers/forIn":3,"./Container":12}],16:[function(e,t){function n(){this.events={}}var o=e("../helpers/ie8/forEach");n.prototype={on:function(e,t,n){this.events[e]||(this.events[e]=[]),this.events[e].push({callback:t,thisArg:n})},emit:function(e,t){var n=this.events[e]||[],i=this.events["*"]||[],r=i.concat(n);o(r,function(n){n.callback.call(n.thisArg,t,e)})},destroy:function(){this.events=void 0}},t.exports=n},{"../helpers/ie8/forEach":5}],17:[function(e,t){var n=e("./Evt"),o=e("./Mediator"),i=e("../helpers/extend"),r=e("../helpers/ie8/bind"),s=e("../helpers/ie8/on"),a=e("../helpers/ie8/off"),h=function(e,t){if(!e)throw new Error("missing target");this.events={},this.target=e,this.settings=i({acceptFromSource:null},t||{}),this.onMessage=r(this.onMessage,this),s(this.target,"message",this.onMessage),this.mediator=new o};h.prototype={onMessage:function(e){if(e.source===this.settings.acceptFromSource){var t=new n(e.data);this.mediator.emit(t.type,t.data)}},on:function(){this.mediator.on.apply(this.mediator,arguments)},destroy:function(){a(this.target,"message",this.onMessage),this.mediator.destroy(),this.mediator=void 0,this.settings=void 0,this.target=void 0,this.events=void 0}},t.exports=h},{"../helpers/extend":1,"../helpers/ie8/bind":4,"../helpers/ie8/off":6,"../helpers/ie8/on":7,"./Evt":13,"./Mediator":16}]},{},[11]);