(function() {
'use strict';
var MaterialTextfield = function MaterialTextfield(element) {
this.element_ = element;
this.maxRows = this.Constant_.NO_MAX_ROWS;
this.init();
};
window['MaterialTextfield'] = MaterialTextfield;
MaterialTextfield.prototype.Constant_ = {
NO_MAX_ROWS: -1,
MAX_ROWS_ATTRIBUTE: 'maxrows'
};
MaterialTextfield.prototype.CssClasses_ = {
LABEL: 'mdl-textfield__label',
INPUT: 'mdl-textfield__input',
IS_DIRTY: 'is-dirty',
IS_FOCUSED: 'is-focused',
IS_DISABLED: 'is-disabled',
IS_INVALID: 'is-invalid',
IS_UPGRADED: 'is-upgraded',
HAS_PLACEHOLDER: 'has-placeholder'
};
MaterialTextfield.prototype.onKeyDown_ = function(event) {
var currentRowCount = event.target.value.split('\n').length;
if (event.keyCode === 13) {
if (currentRowCount >= this.maxRows) {
event.preventDefault();
}
}
};
MaterialTextfield.prototype.onFocus_ = function(event) {
this.element_.classList.add(this.CssClasses_.IS_FOCUSED);
};
MaterialTextfield.prototype.onBlur_ = function(event) {
this.element_.classList.remove(this.CssClasses_.IS_FOCUSED);
};
MaterialTextfield.prototype.onReset_ = function(event) {
this.updateClasses_();
};
MaterialTextfield.prototype.updateClasses_ = function() {
this.checkDisabled();
this.checkValidity();
this.checkDirty();
this.checkFocus();
};
MaterialTextfield.prototype.checkDisabled = function() {
if (this.input_.disabled) {
this.element_.classList.add(this.CssClasses_.IS_DISABLED);
} else {
this.element_.classList.remove(this.CssClasses_.IS_DISABLED);
}
};
MaterialTextfield.prototype['checkDisabled'] =
MaterialTextfield.prototype.checkDisabled;
MaterialTextfield.prototype.checkFocus = function() {
if (Boolean(this.element_.querySelector(':focus'))) {
this.element_.classList.add(this.CssClasses_.IS_FOCUSED);
} else {
this.element_.classList.remove(this.CssClasses_.IS_FOCUSED);
}
};
MaterialTextfield.prototype['checkFocus'] =
MaterialTextfield.prototype.checkFocus;
MaterialTextfield.prototype.checkValidity = function() {
if (this.input_.validity) {
if (this.input_.validity.valid) {
this.element_.classList.remove(this.CssClasses_.IS_INVALID);
} else {
this.element_.classList.add(this.CssClasses_.IS_INVALID);
}
}
};
MaterialTextfield.prototype['checkValidity'] =
MaterialTextfield.prototype.checkValidity;
MaterialTextfield.prototype.checkDirty = function() {
if (this.input_.value && this.input_.value.length > 0) {
this.element_.classList.add(this.CssClasses_.IS_DIRTY);
} else {
this.element_.classList.remove(this.CssClasses_.IS_DIRTY);
}
};
MaterialTextfield.prototype['checkDirty'] =
MaterialTextfield.prototype.checkDirty;
MaterialTextfield.prototype.disable = function() {
this.input_.disabled = true;
this.updateClasses_();
};
MaterialTextfield.prototype['disable'] = MaterialTextfield.prototype.disable;
MaterialTextfield.prototype.enable = function() {
this.input_.disabled = false;
this.updateClasses_();
};
MaterialTextfield.prototype['enable'] = MaterialTextfield.prototype.enable;
MaterialTextfield.prototype.change = function(value) {
this.input_.value = value || '';
this.updateClasses_();
};
MaterialTextfield.prototype['change'] = MaterialTextfield.prototype.change;
MaterialTextfield.prototype.init = function() {
if (this.element_) {
this.label_ = this.element_.querySelector('.' + this.CssClasses_.LABEL);
this.input_ = this.element_.querySelector('.' + this.CssClasses_.INPUT);
if (this.input_) {
if (this.input_.hasAttribute(
(this.Constant_.MAX_ROWS_ATTRIBUTE))) {
this.maxRows = parseInt(this.input_.getAttribute(
(this.Constant_.MAX_ROWS_ATTRIBUTE)), 10);
if (isNaN(this.maxRows)) {
this.maxRows = this.Constant_.NO_MAX_ROWS;
}
}
if (this.input_.hasAttribute('placeholder')) {
this.element_.classList.add(this.CssClasses_.HAS_PLACEHOLDER);
}
this.boundUpdateClassesHandler = this.updateClasses_.bind(this);
this.boundFocusHandler = this.onFocus_.bind(this);
this.boundBlurHandler = this.onBlur_.bind(this);
this.boundResetHandler = this.onReset_.bind(this);
this.input_.addEventListener('input', this.boundUpdateClassesHandler);
this.input_.addEventListener('focus', this.boundFocusHandler);
this.input_.addEventListener('blur', this.boundBlurHandler);
this.input_.addEventListener('reset', this.boundResetHandler);
if (this.maxRows !== this.Constant_.NO_MAX_ROWS) {
this.boundKeyDownHandler = this.onKeyDown_.bind(this);
this.input_.addEventListener('keydown', this.boundKeyDownHandler);
}
var invalid = this.element_.classList
.contains(this.CssClasses_.IS_INVALID);
this.updateClasses_();
this.element_.classList.add(this.CssClasses_.IS_UPGRADED);
if (invalid) {
this.element_.classList.add(this.CssClasses_.IS_INVALID);
}
if (this.input_.hasAttribute('autofocus')) {
this.element_.focus();
this.checkFocus();
}
}
}
};
componentHandler.register({
constructor: MaterialTextfield,
classAsString: 'MaterialTextfield',
cssClass: 'mdl-js-textfield',
widget: true
});
})();