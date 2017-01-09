define([],function(){"use strict";var e=e||{};return e.EXTENSION_ID="kmendfapggjehodndflmmgagdbamhnfd",e.MessageTypes={U2F_REGISTER_REQUEST:"u2f_register_request",U2F_SIGN_REQUEST:"u2f_sign_request",U2F_REGISTER_RESPONSE:"u2f_register_response",U2F_SIGN_RESPONSE:"u2f_sign_response"},e.ErrorCodes={OK:0,OTHER_ERROR:1,BAD_REQUEST:2,CONFIGURATION_UNSUPPORTED:3,DEVICE_INELIGIBLE:4,TIMEOUT:5},e.Request,e.Response,e.Error,e.SignRequest,e.SignResponse,e.RegisterRequest,e.RegisterResponse,e.getMessagePort=function(t){if("undefined"!=typeof chrome&&chrome.runtime){var r={type:e.MessageTypes.U2F_SIGN_REQUEST,signRequests:[]};chrome.runtime.sendMessage(e.EXTENSION_ID,r,function(){chrome.runtime.lastError?e.getIframePort_(t):e.getChromeRuntimePort_(t)})}else e.getIframePort_(t)},e.getChromeRuntimePort_=function(t){var r=chrome.runtime.connect(e.EXTENSION_ID,{includeTlsChannelId:!0});setTimeout(function(){t(new e.WrappedChromeRuntimePort_(r))},0)},e.WrappedChromeRuntimePort_=function(e){this.port_=e},e.WrappedChromeRuntimePort_.prototype.postMessage=function(e){this.port_.postMessage(e)},e.WrappedChromeRuntimePort_.prototype.addEventListener=function(e,t){var r=e.toLowerCase();"message"==r||"onmessage"==r?this.port_.onMessage.addListener(function(e){t({data:e})}):console.error("WrappedChromeRuntimePort only supports onMessage")},e.getIframePort_=function(t){var r="chrome-extension://"+e.EXTENSION_ID,n=document.createElement("iframe");n.src=r+"/u2f-comms.html",n.setAttribute("style","display:none"),document.body.appendChild(n);var o=new MessageChannel,s=function(e){"ready"==e.data?(o.port1.removeEventListener("message",s),t(o.port1)):console.error('First event on iframe port was not "ready"')};o.port1.addEventListener("message",s),o.port1.start(),n.addEventListener("load",function(){n.contentWindow.postMessage("init",r,[o.port2])})},e.EXTENSION_TIMEOUT_SEC=30,e.port_=null,e.waitingForPort_=[],e.reqCounter_=0,e.callbackMap_={},e.getPortSingleton_=function(t){e.port_?t(e.port_):(0==e.waitingForPort_.length&&e.getMessagePort(function(t){for(e.port_=t,e.port_.addEventListener("message",e.responseHandler_);e.waitingForPort_.length;)e.waitingForPort_.shift()(e.port_)}),e.waitingForPort_.push(t))},e.responseHandler_=function(t){var r=t.data,n=r.requestId;if(!n||!e.callbackMap_[n])return void console.error("Unknown or missing requestId in response.");var o=e.callbackMap_[n];delete e.callbackMap_[n],o(r.responseData)},e.sign=function(t,r,n){e.getPortSingleton_(function(o){var s=++e.reqCounter_;e.callbackMap_[s]=r;var a={type:e.MessageTypes.U2F_SIGN_REQUEST,signRequests:t,timeoutSeconds:"undefined"!=typeof n?n:e.EXTENSION_TIMEOUT_SEC,requestId:s};o.postMessage(a)})},e.register=function(t,r,n,o){e.getPortSingleton_(function(s){var a=++e.reqCounter_;e.callbackMap_[a]=n;var i={type:e.MessageTypes.U2F_REGISTER_REQUEST,signRequests:r,registerRequests:t,timeoutSeconds:"undefined"!=typeof o?o:e.EXTENSION_TIMEOUT_SEC,requestId:a};s.postMessage(i)})},window.u2f=e,e});
//# sourceMappingURL=u2f-api.min.js-vflbmLPf3.map