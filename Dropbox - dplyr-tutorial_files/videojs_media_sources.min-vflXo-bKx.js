!function(e){var t,r=0,i=e.MediaSource||e.WebKitMediaSource||{},o=e.URL||{},n=/video\/flv(;\s*codecs=["']vp6,aac["'])?$/,s="blob:vjs-media-source/";t=function(){},t.prototype.init=function(){this.listeners=[]},t.prototype.addEventListener=function(e,t){this.listeners[e]||(this.listeners[e]=[]),this.listeners[e].unshift(t)},t.prototype.removeEventListener=function(e,t){for(var r=this.listeners[e],i=r.length;i--;)if(r[i]===t)return r.splice(i,1)},t.prototype.trigger=function(e){for(var t=this.listeners[e.type]||[],r=t.length;r--;)t[r](e)},videojs.MediaSource=function(){var e=this;videojs.MediaSource.prototype.init.call(this),this.sourceBuffers=[],this.readyState="closed",this.listeners={sourceopen:[function(t){e.swfObj=document.getElementById(t.swfId),e.readyState="open",e.swfObj&&e.swfObj.vjs_load()}],webkitsourceopen:[function(t){e.trigger({type:"sourceopen"})}]}},videojs.MediaSource.prototype=new t,videojs.MediaSource.BYTES_PER_SECOND_GOAL=4194304,videojs.MediaSource.TICKS_PER_SECOND=60,videojs.MediaSource.prototype.addSourceBuffer=function(e){var t;if(n.test(e))t=new videojs.SourceBuffer(this);else{if(!this.nativeSource)throw new Error("NotSupportedError (Video.js)");t=this.nativeSource.addSourceBuffer.apply(this.nativeSource,arguments)}return this.sourceBuffers.push(t),t},videojs.MediaSource.prototype.endOfStream=function(){this.swfObj.vjs_endOfStream(),this.readyState="ended"},videojs.mediaSources={},videojs.MediaSource.open=function(e,t){var r=videojs.mediaSources[e];if(!r)throw new Error("Media Source not found (Video.js)");r.trigger({type:"sourceopen",swfId:t})},videojs.SourceBuffer=function(t){var r=this,i=[],o=0,n=function(t){e.setTimeout(t,Math.ceil(1e3/videojs.MediaSource.TICKS_PER_SECOND))},s=function(){var t,u,a,c,d,f="";if(i.length){for(d=document.hidden?videojs.MediaSource.BYTES_PER_SECOND_GOAL:Math.ceil(videojs.MediaSource.BYTES_PER_SECOND_GOAL/videojs.MediaSource.TICKS_PER_SECOND),c=new Uint8Array(Math.min(d,o)),u=c.byteLength;u;)t=i[0].subarray(0,u),c.set(t,c.byteLength-u),t.byteLength<i[0].byteLength?i[0]=i[0].subarray(u):i.shift(),u-=t.byteLength;for(o-=c.byteLength,0!==o?n(s):r.trigger({type:"updateend"}),u=0,a=c.byteLength;a>u;u++)f+=String.fromCharCode(c[u]);b64str=e.btoa(f),r.source.swfObj.CallFunction('<invoke name="vjs_appendBuffer"returntype="javascript"><arguments><string>'+b64str+"</string></arguments></invoke>")}};videojs.SourceBuffer.prototype.init.call(this),this.source=t,this.appendBuffer=function(e){0===i.length&&n(s),this.trigger({type:"update"}),i.push(e),o+=e.byteLength},this.abort=function(){i=[],o=0,this.source.swfObj.vjs_abort()}},videojs.SourceBuffer.prototype=new t,videojs.URL={createObjectURL:function(e){var t=s+r;return r++,videojs.mediaSources[t]=e,t}},videojs.plugin("mediaSource",function(e){var t=this;t.on("loadstart",function(){var e,r=t.currentSrc(),n=function(t){e.trigger(t)};"Html5"===t.techName&&0===r.indexOf(s)&&(e=videojs.mediaSources[r],e.nativeUrl||(e.nativeSource=new i,e.nativeSource.addEventListener("sourceopen",n,!1),e.nativeSource.addEventListener("webkitsourceopen",n,!1),e.nativeUrl=o.createObjectURL(e.nativeSource)),t.src(e.nativeUrl))})})}(this);
//# sourceMappingURL=videojs_media_sources.min.js-vflEbRW7r.map