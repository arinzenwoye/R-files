define(["require","exports","modules/core/browser","modules/clean/history","modules/clean/comments/action_creators"],function(e,n,r,o,t){"use strict";function i(){var e=r.get_uri();return e.getQuery().cak}function u(){var e=r.get_uri();e.removeQuery("cak"),window.history.replaceState(null,null,e.toString())}function c(){var e=i();e&&(t.revealComment(e),u())}function l(){u()}function a(){u()}n.onPreviewAndCommentsReady=c,n.onFileViewerFlip=l,n.onFileViewerClose=a});
//# sourceMappingURL=url_handler.min.js-vflcpVu6q.map