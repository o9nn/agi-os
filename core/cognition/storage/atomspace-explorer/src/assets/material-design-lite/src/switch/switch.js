(function() {
'use strict';
var MaterialSwitch = function MaterialSwitch(element) {
this.element_ = element;
this.init();
};
window['MaterialSwitch'] = MaterialSwitch;
MaterialSwitch.prototype.Constant_ = {
TINY_TIMEOUT: 0.001
};
MaterialSwitch.prototype.CssClasses_ = {
INPUT: 'mdl-switch__input',
TRACK: 'mdl-switch__track',
THUMB: 'mdl-switch__thumb',
FOCUS_HELPER: 'mdl-switch__focus-helper',
RIPPLE_EFFECT: 'mdl-js-ripple-effect',
RIPPLE_IGNORE_EVENTS: 'mdl-js-ripple-effect--ignore-events',
RIPPLE_CONTAINER: 'mdl-switch__ripple-container',
RIPPLE_CENTER: 'mdl-ripple--center',
RIPPLE: 'mdl-ripple',
IS_FOCUSED: 'is-focused',
IS_DISABLED: 'is-disabled',
IS_CHECKED: 'is-checked'
};
MaterialSwitch.prototype.onChange_ = function(event) {
this.updateClasses_();
};
MaterialSwitch.prototype.onFocus_ = function(event) {
this.element_.classList.add(this.CssClasses_.IS_FOCUSED);
};
MaterialSwitch.prototype.onBlur_ = function(event) {
this.element_.classList.remove(this.CssClasses_.IS_FOCUSED);
};
MaterialSwitch.prototype.onMouseUp_ = function(event) {
this.blur_();
};
MaterialSwitch.prototype.updateClasses_ = function() {
this.checkDisabled();
this.checkToggleState();
};
MaterialSwitch.prototype.blur_ = function() {
window.setTimeout(function() {
this.inputElement_.blur();
}.bind(this),  (this.Constant_.TINY_TIMEOUT));
};
MaterialSwitch.prototype.checkDisabled = function() {
if (this.inputElement_.disabled) {
this.element_.classList.add(this.CssClasses_.IS_DISABLED);
} else {
this.element_.classList.remove(this.CssClasses_.IS_DISABLED);
}
};
MaterialSwitch.prototype['checkDisabled'] =
MaterialSwitch.prototype.checkDisabled;
MaterialSwitch.prototype.checkToggleState = function() {
if (this.inputElement_.checked) {
this.element_.classList.add(this.CssClasses_.IS_CHECKED);
} else {
this.element_.classList.remove(this.CssClasses_.IS_CHECKED);
}
};
MaterialSwitch.prototype['checkToggleState'] =
MaterialSwitch.prototype.checkToggleState;
MaterialSwitch.prototype.disable = function() {
this.inputElement_.disabled = true;
this.updateClasses_();
};
MaterialSwitch.prototype['disable'] = MaterialSwitch.prototype.disable;
MaterialSwitch.prototype.enable = function() {
this.inputElement_.disabled = false;
this.updateClasses_();
};
MaterialSwitch.prototype['enable'] = MaterialSwitch.prototype.enable;
MaterialSwitch.prototype.on = function() {
this.inputElement_.checked = true;
this.updateClasses_();
};
MaterialSwitch.prototype['on'] = MaterialSwitch.prototype.on;
MaterialSwitch.prototype.off = function() {
this.inputElement_.checked = false;
this.updateClasses_();
};
MaterialSwitch.prototype['off'] = MaterialSwitch.prototype.off;
MaterialSwitch.prototype.init = function() {
if (this.element_) {
this.inputElement_ = this.element_.querySelector('.' +
this.CssClasses_.INPUT);
var track = document.createElement('div');
track.classList.add(this.CssClasses_.TRACK);
var thumb = document.createElement('div');
thumb.classList.add(this.CssClasses_.THUMB);
var focusHelper = document.createElement('span');
focusHelper.classList.add(this.CssClasses_.FOCUS_HELPER);
thumb.appendChild(focusHelper);
this.element_.appendChild(track);
this.element_.appendChild(thumb);
this.boundMouseUpHandler = this.onMouseUp_.bind(this);
if (this.element_.classList.contains(
this.CssClasses_.RIPPLE_EFFECT)) {
this.element_.classList.add(
this.CssClasses_.RIPPLE_IGNORE_EVENTS);
this.rippleContainerElement_ = document.createElement('span');
this.rippleContainerElement_.classList.add(
this.CssClasses_.RIPPLE_CONTAINER);
this.rippleContainerElement_.classList.add(this.CssClasses_.RIPPLE_EFFECT);
this.rippleContainerElement_.classList.add(this.CssClasses_.RIPPLE_CENTER);
this.rippleContainerElement_.addEventListener('mouseup', this.boundMouseUpHandler);
var ripple = document.createElement('span');
ripple.classList.add(this.CssClasses_.RIPPLE);
this.rippleContainerElement_.appendChild(ripple);
this.element_.appendChild(this.rippleContainerElement_);
}
this.boundChangeHandler = this.onChange_.bind(this);
this.boundFocusHandler = this.onFocus_.bind(this);
this.boundBlurHandler = this.onBlur_.bind(this);
this.inputElement_.addEventListener('change', this.boundChangeHandler);
this.inputElement_.addEventListener('focus', this.boundFocusHandler);
this.inputElement_.addEventListener('blur', this.boundBlurHandler);
this.element_.addEventListener('mouseup', this.boundMouseUpHandler);
this.updateClasses_();
this.element_.classList.add('is-upgraded');
}
};
componentHandler.register({
constructor: MaterialSwitch,
classAsString: 'MaterialSwitch',
cssClass: 'mdl-js-switch',
widget: true
});
})();