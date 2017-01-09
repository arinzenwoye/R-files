!function(){if(window.WEB_SOCKET_FORCE_FLASH);else{if(window.WebSocket)return;if(window.MozWebSocket)return void(window.WebSocket=MozWebSocket)}var e;return e=window.WEB_SOCKET_LOGGER?WEB_SOCKET_LOGGER:window.console&&window.console.log&&window.console.error?window.console:{log:function(){},error:function(){}},flashVersion=swfobject.getFlashPlayerVersion(),flashVersion.major<10||10==flashVersion.major&&flashVersion.minor<3?void e.error("Flash Player >= 10.3.0 is required."):("file:"==location.protocol&&e.error("WARNING: web-socket-js doesn't work in file:///... URL unless you set Flash Security Settings properly. Open the page via Web server i.e. http://..."),window.WebSocket=function(e,t,o,n){var i=this;i.__id=WebSocket.__nextId++,WebSocket.__instances[i.__id]=i,i.readyState=WebSocket.CONNECTING,i.bufferedAmount=0,i.__events={},t?"string"==typeof t&&(t=[t]):t=[],i.__createTask=setTimeout(function(){WebSocket.__addTask(function(){i.__createTask=null,WebSocket.__flash.create(i.__id,e,t,o||null,n||0)})},0)},WebSocket.prototype.send=function(e){if(this.readyState==WebSocket.CONNECTING)throw"INVALID_STATE_ERR: Web Socket connection has not been established";var t=WebSocket.__flash.send(this.__id,encodeURIComponent(e));return 0>t?!0:(this.bufferedAmount+=t,!1)},WebSocket.prototype.close=function(){return this.__createTask?(clearTimeout(this.__createTask),this.__createTask=null,void(this.readyState=WebSocket.CLOSED)):void(this.readyState!=WebSocket.CLOSED&&this.readyState!=WebSocket.CLOSING&&(this.readyState=WebSocket.CLOSING,WebSocket.__flash.close(this.__id)))},WebSocket.prototype.addEventListener=function(e,t,o){e in this.__events||(this.__events[e]=[]),this.__events[e].push(t)},WebSocket.prototype.removeEventListener=function(e,t,o){if(e in this.__events)for(var n=this.__events[e],i=n.length-1;i>=0;--i)if(n[i]===t){n.splice(i,1);break}},WebSocket.prototype.dispatchEvent=function(e){for(var t=this.__events[e.type]||[],o=0;o<t.length;++o)t[o](e);var n=this["on"+e.type];n&&n.apply(this,[e])},WebSocket.prototype.__handleEvent=function(e){"readyState"in e&&(this.readyState=e.readyState),"protocol"in e&&(this.protocol=e.protocol);var t;if("open"==e.type||"error"==e.type)t=this.__createSimpleEvent(e.type);else if("close"==e.type)t=this.__createSimpleEvent("close"),t.wasClean=!!e.wasClean,t.code=e.code,t.reason=e.reason;else{if("message"!=e.type)throw"unknown event type: "+e.type;var o=decodeURIComponent(e.message);t=this.__createMessageEvent("message",o)}this.dispatchEvent(t)},WebSocket.prototype.__createSimpleEvent=function(e){if(document.createEvent&&window.Event){var t=document.createEvent("Event");return t.initEvent(e,!1,!1),t}return{type:e,bubbles:!1,cancelable:!1}},WebSocket.prototype.__createMessageEvent=function(e,t){var o;if(window.MessageEvent&&"function"==typeof MessageEvent&&!window.opera)try{o=new MessageEvent("message",{view:window,bubbles:!1,cancelable:!1,data:t})}catch(e){}return o?o:document.createEvent&&window.MessageEvent&&!window.opera?(o=document.createEvent("MessageEvent"),o.initMessageEvent("message",!1,!1,t,null,null,window,null),o):{type:e,data:t,bubbles:!1,cancelable:!1}},WebSocket.CONNECTING=0,WebSocket.OPEN=1,WebSocket.CLOSING=2,WebSocket.CLOSED=3,WebSocket.__isFlashImplementation=!0,WebSocket.__initialized=!1,WebSocket.__flash=null,WebSocket.__instances={},WebSocket.__tasks=[],WebSocket.__nextId=0,WebSocket.loadFlashPolicyFile=function(e){WebSocket.__addTask(function(){WebSocket.__flash.loadManualPolicyFile(e)})},WebSocket.__initialize=function(){if(!WebSocket.__initialized){if(WebSocket.__initialized=!0,WebSocket.__swfLocation&&(window.WEB_SOCKET_SWF_LOCATION=WebSocket.__swfLocation),!window.WEB_SOCKET_SWF_LOCATION)return void e.error("[WebSocket] set WEB_SOCKET_SWF_LOCATION to location of WebSocket.swf");var t=document.createElement("div");t.id="webSocketContainer",t.style.position="absolute",WebSocket.__isFlashLite()?(t.style.left="0px",t.style.top="0px"):(t.style.left="-100px",t.style.top="-100px");var o=document.createElement("div");o.id="webSocketFlash",t.appendChild(o),document.body.appendChild(t),swfobject.embedSWF(WEB_SOCKET_SWF_LOCATION,"webSocketFlash","1","1","10.3.0",null,null,{hasPriority:!0,swliveconnect:!0,allowScriptAccess:"always"},null,function(t){t.success||e.error("[WebSocket] swfobject.embedSWF failed")})}},WebSocket.__onFlashInitialized=function(){setTimeout(function(){WebSocket.__flash=document.getElementById("webSocketFlash"),WebSocket.__flash.setDebug(!!window.WEB_SOCKET_DEBUG);for(var e=0;e<WebSocket.__tasks.length;++e)WebSocket.__tasks[e]();WebSocket.__tasks=[]},0)},WebSocket.__onFlashEvent=function(){return setTimeout(function(){try{for(var t=WebSocket.__flash.receiveEvents(),o=0;o<t.length;++o)WebSocket.__instances[t[o].webSocketId].__handleEvent(t[o])}catch(t){e.error(t)}},0),!0},WebSocket.__log=function(t){e.log(decodeURIComponent(t))},WebSocket.__error=function(t){e.error(decodeURIComponent(t))},WebSocket.__addTask=function(e){WebSocket.__flash?e():WebSocket.__tasks.push(e)},WebSocket.__isFlashLite=function(){if(!window.navigator||!window.navigator.mimeTypes)return!1;var e=window.navigator.mimeTypes["application/x-shockwave-flash"];return e&&e.enabledPlugin&&e.enabledPlugin.filename?!!e.enabledPlugin.filename.match(/flashlite/i):!1},void(window.WEB_SOCKET_DISABLE_AUTO_INITIALIZATION||swfobject.addDomLoadEvent(function(){WebSocket.__initialize()})))}();
//# sourceMappingURL=web_socket.min.js-vflwKE-NE.map