(function(){var n=[].indexOf||function(n){for(var t=0,e=this.length;e>t;t++)if(t in this&&this[t]===n)return t;return-1};define(["external/react","modules/clean/referrer_cleansing_redirect"],function(t,e){var r,l;return r=function(r,u,i,h){var s,a,c,o,p,f,d,g,m,v,_,x,b,w,z,O,R,A;for(null==u&&(u=!0),null==i&&(i=!1),null==h&&(h=null),a=t.DOM,d=/@\[(?:\s*(?:dbid:)?[^:\[\]]+):(?:[^\[]+)\]/i,f=/@\[(\s*(?:dbid:)?[^:\[\]]+):([^\[]+)\]/i,A="(?:[-a-zA-Z0-9@:%_+~#=]+[.])+[a-z]{2,6}(?:[:./?#][-a-zA-Z0-9@%_+~&=]+)*(?=[^a-zA-Z0-9_]|$)",O="http://"+A+"|https://"+A+"|www."+A+"|"+A,R=new RegExp(O,"i"),p=/\n/,x=[d,p],u&&x.push(R),b=RegExp("("+function(){var n,t,e;for(e=[],n=0,t=x.length;t>n;n++)v=x[n],e.push(v.source);return e}().join("|")+")","gi"),m=[],_=r.split(b),c=0,o=_.length;o>c;c++)s=_[c],s&&(s.match(d)?(w=s.match(f),g=w[2].trim(),g=i?"@"+g:g,m.push(a.span({className:"comment-mention"},g))):n.call(x,R)>=0&&s.match(R)?(z=s,z.indexOf("http://")<0&&z.indexOf("https://")<0&&(z="http://"+z),z=e.get_redirect_uri(z),m.push(a.a({href:z,target:"_blank"},s))):s.match(p)?m.push(a.br({})):m.push(s));return l(m,h)},l=function(n,t){var e,r,l,u,i,h,s,a,c,o;if(null==t&&(t=null),!t)return n;if(r=" …",e=0,o=[],0>=t)return n.length>0?r:n;for(l=0,i=n.length;i>l;l++)if(c=n[l],"object"==typeof c){if(null!=(null!=(h=c.props)?h.children:void 0)&&(e+=null!=(s=c.props)&&null!=(a=s.children)?a.length:void 0),o.push(c),e>=t)return o.push(r),o}else{if(e+c.length>=t)return u=t-e,o.push(c.substring(0,u)),o.push(r),o;e+=c.length,o.push(c)}return n},{convertRawCommentTextToReactDom:r}})}).call(this);
//# sourceMappingURL=comment_dom.min.js-vflNnjQIu.map