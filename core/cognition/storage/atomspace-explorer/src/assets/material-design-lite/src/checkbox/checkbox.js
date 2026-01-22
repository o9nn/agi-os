(function() {
'use strict';
var MaterialCheckbox = function MaterialCheckbox(element) {
this.element_ = element;
this.init();
};
window['MaterialCheckbox'] = MaterialCheckbox;
MaterialCheckbox.prototype.Constant_ = {
TINY_TIMEOUT: 0.001
};
MaterialCheckbox.prototype.CssClasses_ = {
INPUT: 'mdl-checkbox__input',
BOX_OUTLINE: 'mdl-checkbox__box-outline',
FOCUS_HELPER: 'mdl-checkbox__focus-helper',
TICK_OUTLINE: 'mdl-checkbox__tick-outline',
RIPPLE_EFFECT: 'mdl-js-ripple-effect',
RIPPLE_IGNORE_EVENTS: 'mdl-js-ripple-effect--ignore-events',
RIPPLE_CONTAINER: 'mdl-checkbox__ripple-container',
RIPPLE_CENTER: 'mdl-ripple--center',
RIPPLE: 'mdl-ripple',
IS_FOCUSED: 'is-focused',
IS_DISABLED: 'is-disabled',
IS_CHECKED: 'is-checked',
IS_UPGRADED: 'is-upgraded'
};
MaterialCheckbox.prototype.onChange_ = function(event) {
this.updateClasses_();
};
MaterialCheckbox.prototype.onFocus_ = function(event) {
this.element_.classList.add(this.CssClasses_.IS_FOCUSED);
};
MaterialCheckbox.prototype.onBlur_ = function(event) {
this.element_.classList.remove(this.CssClasses_.IS_FOCUSED);
};
MaterialCheckbox.prototype.onMouseUp_ = function(event) {
this.blur_();
};
MaterialCheckbox.prototype.updateClasses_ = function() {
this.checkDisabled();
this.checkToggleState();
};
MaterialCheckbox.prototype.blur_ = function() {
window.setTimeout(function() {
this.inputElement_.blur();
}.bind(this),  (this.Constant_.TINY_TIMEOUT));
};
MaterialCheckbox.prototype.checkToggleState = function() {
if (this.inputElement_.checked) {
this.element_.classList.add(this.CssClasses_.IS_CHECKED);
} else {
this.element_.classList.remove(this.CssClasses_.IS_CHECKED);
}
};
MaterialCheckbox.prototype['checkToggleState'] =
MaterialCheckbox.prototype.checkToggleState;
MaterialCheckbox.prototype.checkDisabled = function() {
if (this.inputElement_.disabled) {
this.element_.classList.add(this.CssClasses_.IS_DISABLED);
} else {
this.element_.classList.remove(this.CssClasses_.IS_DISABLED);
}
};
MaterialCheckbox.prototype['checkDisabled'] =
MaterialCheckbox.prototype.checkDisabled;
MaterialCheckbox.prototype.disable = function() {
this.inputElement_.disabled = true;
this.updateClasses_();
};
MaterialCheckbox.prototype['disable'] = MaterialCheckbox.prototype.disable;
MaterialCheckbox.prototype.enable = function() {
this.inputElement_.disabled = false;
this.updateClasses_();
};
MaterialCheckbox.prototype['enable'] = MaterialCheckbox.prototype.enable;
MaterialCheckbox.prototype.check = function() {
this.inputElement_.checked = true;
this.updateClasses_();
};
MaterialCheckbox.prototype['check'] = MaterialCheckbox.prototype.check;
MaterialCheckbox.prototype.uncheck = function() {
this.inputElement_.checked = false;
this.updateClasses_();
};
MaterialCheckbox.prototype['uncheck'] = MaterialCheckbox.prototype.uncheck;
MaterialCheckbox.prototype.init = function() {
if (this.element_) {
this.inputElement_ = this.element_.querySelector('.' +
this.CssClasses_.INPUT);
var boxOutline = document.createElement('span');
boxOutline.classList.add(this.CssClasses_.BOX_OUTLINE);
var tickContainer = document.createElement('span');
tickContainer.classList.add(this.CssClasses_.FOCUS_HELPER);
var tickOutline = document.createElement('span');
tickOutline.classList.add(this.CssClasses_.TICK_OUTLINE);
boxOutline.appendChild(tickOutline);
this.element_.appendChild(tickContainer);
this.element_.appendChild(boxOutline);
if (this.element_.classList.contains(this.CssClasses_.RIPPLE_EFFECT)) {
this.element_.classList.add(this.CssClasses_.RIPPLE_IGNORE_EVENTS);
this.rippleContainerElement_ = document.createElement('span');
this.rippleContainerElement_.classList.add(this.CssClasses_.RIPPLE_CONTAINER);
this.rippleContainerElement_.classList.add(this.CssClasses_.RIPPLE_EFFECT);
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
this.boundElementMouseUp = this.onMouseUp_.bind(this);
this.inputElement_.addEventListener('change', this.boundInputOnChange);
this.inputElement_.addEventListener('focus', this.boundInputOnFocus);
this.inputElement_.addEventListener('blur', this.boundInputOnBlur);
this.element_.addEventListener('mouseup', this.boundElementMouseUp);
this.updateClasses_();
this.element_.classList.add(this.CssClasses_.IS_UPGRADED);
}
};
componentHandler.register({
constructor: MaterialCheckbox,
classAsString: 'MaterialCheckbox',
cssClass: 'mdl-js-checkbox',
widget: true
});
})();