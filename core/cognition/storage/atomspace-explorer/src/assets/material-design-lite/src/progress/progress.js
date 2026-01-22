(function() {
'use strict';
var MaterialProgress = function MaterialProgress(element) {
this.element_ = element;
this.init();
};
window['MaterialProgress'] = MaterialProgress;
MaterialProgress.prototype.Constant_ = {
};
MaterialProgress.prototype.CssClasses_ = {
INDETERMINATE_CLASS: 'mdl-progress__indeterminate'
};
MaterialProgress.prototype.setProgress = function(p) {
if (this.element_.classList.contains(this.CssClasses_.INDETERMINATE_CLASS)) {
return;
}
this.progressbar_.style.width = p + '%';
};
MaterialProgress.prototype['setProgress'] =
MaterialProgress.prototype.setProgress;
MaterialProgress.prototype.setBuffer = function(p) {
this.bufferbar_.style.width = p + '%';
this.auxbar_.style.width = (100 - p) + '%';
};
MaterialProgress.prototype['setBuffer'] =
MaterialProgress.prototype.setBuffer;
MaterialProgress.prototype.init = function() {
if (this.element_) {
var el = document.createElement('div');
el.className = 'progressbar bar bar1';
this.element_.appendChild(el);
this.progressbar_ = el;
el = document.createElement('div');
el.className = 'bufferbar bar bar2';
this.element_.appendChild(el);
this.bufferbar_ = el;
el = document.createElement('div');
el.className = 'auxbar bar bar3';
this.element_.appendChild(el);
this.auxbar_ = el;
this.progressbar_.style.width = '0%';
this.bufferbar_.style.width = '100%';
this.auxbar_.style.width = '0%';
this.element_.classList.add('is-upgraded');
}
};
componentHandler.register({
constructor: MaterialProgress,
classAsString: 'MaterialProgress',
cssClass: 'mdl-js-progress',
widget: true
});
})();