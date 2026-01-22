(function() {
'use strict';
var MaterialIconToggle = function MaterialIconToggle(element) {
this.element_ = element;
this.init();
};
window['MaterialIconToggle'] = MaterialIconToggle;
MaterialIconToggle.prototype.Constant_ = {
TINY_TIMEOUT: 0.001
};
MaterialIconToggle.prototype.CssClasses_ = {
INPUT: 'mdl-icon-toggle__input',
JS_RIPPLE_EFFECT: 'mdl-js-ripple-effect',
RIPPLE_IGNORE_EVENTS: 'mdl-js-ripple-effect--ignore-events',
RIPPLE_CONTAINER: 'mdl-icon-toggle__ripple-container',
RIPPLE_CENTER: 'mdl-ripple--center',
RIPPLE: 'mdl-ripple',
IS_FOCUSED: 'is-focused',
IS_DISABLED: 'is-disabled',
IS_CHECKED: 'is-checked'
};
MaterialIconToggle.prototype.onChange_ = function(event) {
this.updateClasses_();
};
MaterialIconToggle.prototype.onFocus_ = function(event) {
this.element_.classList.add(this.CssClasses_.IS_FOCUSED);
};
MaterialIconToggle.prototype.onBlur_ = function(event) {
this.element_.classList.remove(this.CssClasses_.IS_FOCUSED);
};
MaterialIconToggle.prototype.onMouseUp_ = function(event) {
this.blur_();
};
MaterialIconToggle.prototype.updateClasses_ = function() {
this.checkDisabled();
this.checkToggleState();
};
MaterialIconToggle.prototype.blur_ = function() {
window.setTimeout(function() {
this.inputElement_.blur();
}.bind(this),  (this.Constant_.TINY_TIMEOUT));
};
MaterialIconToggle.prototype.checkToggleState = function() {
if (this.inputElement_.checked) {
this.element_.classList.add(this.CssClasses_.IS_CHECKED);
} else {
this.element_.classList.remove(this.CssClasses_.IS_CHECKED);
}
};
MaterialIconToggle.prototype['checkToggleState'] =
MaterialIconToggle.prototype.checkToggleState;
MaterialIconToggle.prototype.checkDisabled = function() {
if (this.inputElement_.disabled) {
this.element_.classList.add(this.CssClasses_.IS_DISABLED);
} else {
this.element_.classList.remove(this.CssClasses_.IS_DISABLED);
}
};
MaterialIconToggle.prototype['checkDisabled'] =
MaterialIconToggle.prototype.checkDisabled;
MaterialIconToggle.prototype.disable = function() {
this.inputElement_.disabled = true;
this.updateClasses_();
};
MaterialIconToggle.prototype['disable'] =
MaterialIconToggle.prototype.disable;
MaterialIconToggle.prototype.enable = function() {
this.inputElement_.disabled = false;
this.updateClasses_();
};
MaterialIconToggle.prototype['enable'] = MaterialIconToggle.prototype.enable;
MaterialIconToggle.prototype.check = function() {
this.inputElement_.checked = true;
this.updateClasses_();
};
MaterialIconToggle.prototype['check'] = MaterialIconToggle.prototype.check;
MaterialIconToggle.prototype.uncheck = function() {
this.inputElement_.checked = false;
this.updateClasses_();
};
MaterialIconToggle.prototype['uncheck'] =
MaterialIconToggle.prototype.uncheck;
MaterialIconToggle.prototype.init = function() {
if (this.element_) {
this.inputElement_ =
this.element_.querySelector('.' + this.CssClasses_.INPUT);
if (this.element_.classList.contains(this.CssClasses_.JS_RIPPLE_EFFECT)) {
this.element_.classList.add(this.CssClasses_.RIPPLE_IGNORE_EVENTS);
this.rippleContainerElement_ = document.createElement('span');
this.rippleContainerElement_.classList.add(this.CssClasses_.RIPPLE_CONTAINER);
this.rippleContainerElement_.classList.add(this.CssClasses_.JS_RIPPLE_EFFECT);
this.rippleContainerElement_.classList.add(this.CssClasses_.RIPPLE_CENTER);
this.boundRippleMouseUp = this.onMouseUp_.bind(this);
this.rippleContainerElement_.addEventListener('mouseup', this.boundRippleMouseUp);
var ripple = document.createElement('span');
ripple.classList.add(this.CssClasses_.RIPPLE);
this.rippleContainerElement_.appendChild(ripple);
this.element_.appendChild(this.rippleContainerElement_);
}
this.boundInputOnChange = this.onChange_.bind(this);
this.boundInputOnFocus = this.onFocus_.bind(this);
this.boundInputOnBlur = this.onBlur_.bind(this);
this.boundElementOnMouseUp = this.onMouseUp_.bind(this);
this.inputElement_.addEventListener('change', this.boundInputOnChange);
this.inputElement_.addEventListener('focus', this.boundInputOnFocus);
this.inputElement_.addEventListener('blur', this.boundInputOnBlur);
this.element_.addEventListener('mouseup', this.boundElementOnMouseUp);
this.updateClasses_();
this.element_.classList.add('is-upgraded');
}
};
componentHandler.register({
constructor: MaterialIconToggle,
classAsString: 'MaterialIconToggle',
cssClass: 'mdl-js-icon-toggle',
widget: true
});
})();