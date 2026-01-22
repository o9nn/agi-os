(function() {
  'use strict';
  var MaterialRadio = function MaterialRadio(element) {
    this.element_ = element;
    this.init();
  };
  window['MaterialRadio'] = MaterialRadio;
  MaterialRadio.prototype.Constant_ = {
    TINY_TIMEOUT: 0.001
  };
  MaterialRadio.prototype.CssClasses_ = {
    IS_FOCUSED: 'is-focused',
    IS_DISABLED: 'is-disabled',
    IS_CHECKED: 'is-checked',
    IS_UPGRADED: 'is-upgraded',
    JS_RADIO: 'mdl-js-radio',
    RADIO_BTN: 'mdl-radio__button',
    RADIO_OUTER_CIRCLE: 'mdl-radio__outer-circle',
    RADIO_INNER_CIRCLE: 'mdl-radio__inner-circle',
    RIPPLE_EFFECT: 'mdl-js-ripple-effect',
    RIPPLE_IGNORE_EVENTS: 'mdl-js-ripple-effect--ignore-events',
    RIPPLE_CONTAINER: 'mdl-radio__ripple-container',
    RIPPLE_CENTER: 'mdl-ripple--center',
    RIPPLE: 'mdl-ripple'
  };
  MaterialRadio.prototype.onChange_ = function(event) {
    var radios = document.getElementsByClassName(this.CssClasses_.JS_RADIO);
    for (var i = 0; i < radios.length; i++) {
      var button = radios[i].querySelector('.' + this.CssClasses_.RADIO_BTN);
      if (button.getAttribute('name') === this.btnElement_.getAttribute('name')) {
        if (typeof radios[i]['MaterialRadio'] !== 'undefined') {
          radios[i]['MaterialRadio'].updateClasses_();
        }
      }
    }
  };
  MaterialRadio.prototype.onFocus_ = function(event) {
    this.element_.classList.add(this.CssClasses_.IS_FOCUSED);
  };
  MaterialRadio.prototype.onBlur_ = function(event) {
    this.element_.classList.remove(this.CssClasses_.IS_FOCUSED);
  };
  MaterialRadio.prototype.onMouseup_ = function(event) {
    this.blur_();
  };
  MaterialRadio.prototype.updateClasses_ = function() {
    this.checkDisabled();
    this.checkToggleState();
  };
  MaterialRadio.prototype.blur_ = function() {
    window.setTimeout(function() {
      this.btnElement_.blur();
    }.bind(this),  (this.Constant_.TINY_TIMEOUT));
  };
  MaterialRadio.prototype.checkDisabled = function() {
    if (this.btnElement_.disabled) {
      this.element_.classList.add(this.CssClasses_.IS_DISABLED);
    } else {
      this.element_.classList.remove(this.CssClasses_.IS_DISABLED);
    }
  };
  MaterialRadio.prototype['checkDisabled'] =
      MaterialRadio.prototype.checkDisabled;
  MaterialRadio.prototype.checkToggleState = function() {
    if (this.btnElement_.checked) {
      this.element_.classList.add(this.CssClasses_.IS_CHECKED);
    } else {
      this.element_.classList.remove(this.CssClasses_.IS_CHECKED);
    }
  };
  MaterialRadio.prototype['checkToggleState'] =
      MaterialRadio.prototype.checkToggleState;
  MaterialRadio.prototype.disable = function() {
    this.btnElement_.disabled = true;
    this.updateClasses_();
  };
  MaterialRadio.prototype['disable'] = MaterialRadio.prototype.disable;
  MaterialRadio.prototype.enable = function() {
    this.btnElement_.disabled = false;
    this.updateClasses_();
  };
  MaterialRadio.prototype['enable'] = MaterialRadio.prototype.enable;
  MaterialRadio.prototype.check = function() {
    this.btnElement_.checked = true;
    this.onChange_(null);
  };
  MaterialRadio.prototype['check'] = MaterialRadio.prototype.check;
  MaterialRadio.prototype.uncheck = function() {
    this.btnElement_.checked = false;
    this.onChange_(null);
  };
  MaterialRadio.prototype['uncheck'] = MaterialRadio.prototype.uncheck;
  MaterialRadio.prototype.init = function() {
    if (this.element_) {
      this.btnElement_ = this.element_.querySelector('.' +
          this.CssClasses_.RADIO_BTN);
      this.boundChangeHandler_ = this.onChange_.bind(this);
      this.boundFocusHandler_ = this.onChange_.bind(this);
      this.boundBlurHandler_ = this.onBlur_.bind(this);
      this.boundMouseUpHandler_ = this.onMouseup_.bind(this);
      var outerCircle = document.createElement('span');
      outerCircle.classList.add(this.CssClasses_.RADIO_OUTER_CIRCLE);
      var innerCircle = document.createElement('span');
      innerCircle.classList.add(this.CssClasses_.RADIO_INNER_CIRCLE);
      this.element_.appendChild(outerCircle);
      this.element_.appendChild(innerCircle);
      var rippleContainer;
      if (this.element_.classList.contains(
          this.CssClasses_.RIPPLE_EFFECT)) {
        this.element_.classList.add(
            this.CssClasses_.RIPPLE_IGNORE_EVENTS);
        rippleContainer = document.createElement('span');
        rippleContainer.classList.add(
            this.CssClasses_.RIPPLE_CONTAINER);
        rippleContainer.classList.add(this.CssClasses_.RIPPLE_EFFECT);
        rippleContainer.classList.add(this.CssClasses_.RIPPLE_CENTER);
        rippleContainer.addEventListener('mouseup', this.boundMouseUpHandler_);
        var ripple = document.createElement('span');
        ripple.classList.add(this.CssClasses_.RIPPLE);
        rippleContainer.appendChild(ripple);
        this.element_.appendChild(rippleContainer);
      }
      this.btnElement_.addEventListener('change', this.boundChangeHandler_);
      this.btnElement_.addEventListener('focus', this.boundFocusHandler_);
      this.btnElement_.addEventListener('blur', this.boundBlurHandler_);
      this.element_.addEventListener('mouseup', this.boundMouseUpHandler_);
      this.updateClasses_();
      this.element_.classList.add(this.CssClasses_.IS_UPGRADED);
    }
  };
  componentHandler.register({
    constructor: MaterialRadio,
    classAsString: 'MaterialRadio',
    cssClass: 'mdl-js-radio',
    widget: true
  });
})();