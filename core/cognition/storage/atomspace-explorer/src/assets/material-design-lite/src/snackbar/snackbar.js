(function() {
'use strict';
var MaterialSnackbar = function MaterialSnackbar(element) {
this.element_ = element;
this.textElement_ = this.element_.querySelector('.' + this.cssClasses_.MESSAGE);
this.actionElement_ = this.element_.querySelector('.' + this.cssClasses_.ACTION);
if (!this.textElement_) {
throw new Error('There must be a message element for a snackbar.');
}
if (!this.actionElement_) {
throw new Error('There must be an action element for a snackbar.');
}
this.active = false;
this.actionHandler_ = undefined;
this.message_ = undefined;
this.actionText_ = undefined;
this.queuedNotifications_ = [];
this.setActionHidden_(true);
};
window['MaterialSnackbar'] = MaterialSnackbar;
MaterialSnackbar.prototype.Constant_ = {
ANIMATION_LENGTH: 250
};
MaterialSnackbar.prototype.cssClasses_ = {
SNACKBAR: 'mdl-snackbar',
MESSAGE: 'mdl-snackbar__text',
ACTION: 'mdl-snackbar__action',
ACTIVE: 'mdl-snackbar--active'
};
MaterialSnackbar.prototype.displaySnackbar_ = function() {
this.element_.setAttribute('aria-hidden', 'true');
if (this.actionHandler_) {
this.actionElement_.textContent = this.actionText_;
this.actionElement_.addEventListener('click', this.actionHandler_);
this.setActionHidden_(false);
}
this.textElement_.textContent = this.message_;
this.element_.classList.add(this.cssClasses_.ACTIVE);
this.element_.setAttribute('aria-hidden', 'false');
setTimeout(this.cleanup_.bind(this), this.timeout_);
};
MaterialSnackbar.prototype.showSnackbar = function(data) {
if (data === undefined) {
throw new Error(
'Please provide a data object with at least a message to display.');
}
if (data['message'] === undefined) {
throw new Error('Please provide a message to be displayed.');
}
if (data['actionHandler'] && !data['actionText']) {
throw new Error('Please provide action text with the handler.');
}
if (this.active) {
this.queuedNotifications_.push(data);
} else {
this.active = true;
this.message_ = data['message'];
if (data['timeout']) {
this.timeout_ = data['timeout'];
} else {
this.timeout_ = 2750;
}
if (data['actionHandler']) {
this.actionHandler_ = data['actionHandler'];
}
if (data['actionText']) {
this.actionText_ = data['actionText'];
}
this.displaySnackbar_();
}
};
MaterialSnackbar.prototype['showSnackbar'] = MaterialSnackbar.prototype.showSnackbar;
MaterialSnackbar.prototype.checkQueue_ = function() {
if (this.queuedNotifications_.length > 0) {
this.showSnackbar(this.queuedNotifications_.shift());
}
};
MaterialSnackbar.prototype.cleanup_ = function() {
this.element_.classList.remove(this.cssClasses_.ACTIVE);
setTimeout(function() {
this.element_.setAttribute('aria-hidden', 'true');
this.textElement_.textContent = '';
if (!Boolean(this.actionElement_.getAttribute('aria-hidden'))) {
this.setActionHidden_(true);
this.actionElement_.textContent = '';
this.actionElement_.removeEventListener('click', this.actionHandler_);
}
this.actionHandler_ = undefined;
this.message_ = undefined;
this.actionText_ = undefined;
this.active = false;
this.checkQueue_();
}.bind(this),  (this.Constant_.ANIMATION_LENGTH));
};
MaterialSnackbar.prototype.setActionHidden_ = function(value) {
if (value) {
this.actionElement_.setAttribute('aria-hidden', 'true');
} else {
this.actionElement_.removeAttribute('aria-hidden');
}
};
componentHandler.register({
constructor: MaterialSnackbar,
classAsString: 'MaterialSnackbar',
cssClass: 'mdl-js-snackbar',
widget: true
});
})();