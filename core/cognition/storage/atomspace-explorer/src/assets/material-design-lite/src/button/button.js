(function() {
'use strict';
var MaterialButton = function MaterialButton(element) {
this.element_ = element;
this.init();
};
window['MaterialButton'] = MaterialButton;
MaterialButton.prototype.Constant_ = {
};
MaterialButton.prototype.CssClasses_ = {
RIPPLE_EFFECT: 'mdl-js-ripple-effect',
RIPPLE_CONTAINER: 'mdl-button__ripple-container',
RIPPLE: 'mdl-ripple'
};
MaterialButton.prototype.blurHandler_ = function(event) {
if (event) {
this.element_.blur();
}
};
MaterialButton.prototype.disable = function() {
this.element_.disabled = true;
};
MaterialButton.prototype['disable'] = MaterialButton.prototype.disable;
MaterialButton.prototype.enable = function() {
this.element_.disabled = false;
};
MaterialButton.prototype['enable'] = MaterialButton.prototype.enable;
MaterialButton.prototype.init = function() {
if (this.element_) {
if (this.element_.classList.contains(this.CssClasses_.RIPPLE_EFFECT)) {
var rippleContainer = document.createElement('span');
rippleContainer.classList.add(this.CssClasses_.RIPPLE_CONTAINER);
this.rippleElement_ = document.createElement('span');
this.rippleElement_.classList.add(this.CssClasses_.RIPPLE);
rippleContainer.appendChild(this.rippleElement_);
this.boundRippleBlurHandler = this.blurHandler_.bind(this);
this.rippleElement_.addEventListener('mouseup', this.boundRippleBlurHandler);
this.element_.appendChild(rippleContainer);
}
this.boundButtonBlurHandler = this.blurHandler_.bind(this);
this.element_.addEventListener('mouseup', this.boundButtonBlurHandler);
this.element_.addEventListener('mouseleave', this.boundButtonBlurHandler);
}
};
componentHandler.register({
constructor: MaterialButton,
classAsString: 'MaterialButton',
cssClass: 'mdl-js-button',
widget: true
});
})();