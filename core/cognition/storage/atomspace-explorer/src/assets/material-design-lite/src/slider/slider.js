(function() {
  'use strict';
  var MaterialSlider = function MaterialSlider(element) {
    this.element_ = element;
    this.isIE_ = window.navigator.msPointerEnabled;
    this.init();
  };
  window['MaterialSlider'] = MaterialSlider;
  MaterialSlider.prototype.Constant_ = {
  };
  MaterialSlider.prototype.CssClasses_ = {
    IE_CONTAINER: 'mdl-slider__ie-container',
    SLIDER_CONTAINER: 'mdl-slider__container',
    BACKGROUND_FLEX: 'mdl-slider__background-flex',
    BACKGROUND_LOWER: 'mdl-slider__background-lower',
    BACKGROUND_UPPER: 'mdl-slider__background-upper',
    IS_LOWEST_VALUE: 'is-lowest-value',
    IS_UPGRADED: 'is-upgraded'
  };
  MaterialSlider.prototype.onInput_ = function(event) {
    this.updateValueStyles_();
  };
  MaterialSlider.prototype.onChange_ = function(event) {
    this.updateValueStyles_();
  };
  MaterialSlider.prototype.onMouseUp_ = function(event) {
    event.target.blur();
  };
  MaterialSlider.prototype.onContainerMouseDown_ = function(event) {
    if (event.target !== this.element_.parentElement) {
      return;
    }
    event.preventDefault();
    var newEvent = new MouseEvent('mousedown', {
      target: event.target,
      buttons: event.buttons,
      clientX: event.clientX,
      clientY: this.element_.getBoundingClientRect().y
    });
    this.element_.dispatchEvent(newEvent);
  };
  MaterialSlider.prototype.updateValueStyles_ = function() {
    var fraction = (this.element_.value - this.element_.min) /
        (this.element_.max - this.element_.min);
    if (fraction === 0) {
      this.element_.classList.add(this.CssClasses_.IS_LOWEST_VALUE);
    } else {
      this.element_.classList.remove(this.CssClasses_.IS_LOWEST_VALUE);
    }
    if (!this.isIE_) {
      this.backgroundLower_.style.flex = fraction;
      this.backgroundLower_.style.webkitFlex = fraction;
      this.backgroundUpper_.style.flex = 1 - fraction;
      this.backgroundUpper_.style.webkitFlex = 1 - fraction;
    }
  };
  MaterialSlider.prototype.disable = function() {
    this.element_.disabled = true;
  };
  MaterialSlider.prototype['disable'] = MaterialSlider.prototype.disable;
  MaterialSlider.prototype.enable = function() {
    this.element_.disabled = false;
  };
  MaterialSlider.prototype['enable'] = MaterialSlider.prototype.enable;
  MaterialSlider.prototype.change = function(value) {
    if (typeof value !== 'undefined') {
      this.element_.value = value;
    }
    this.updateValueStyles_();
  };
  MaterialSlider.prototype['change'] = MaterialSlider.prototype.change;
  MaterialSlider.prototype.init = function() {
    if (this.element_) {
      if (this.isIE_) {
        var containerIE = document.createElement('div');
        containerIE.classList.add(this.CssClasses_.IE_CONTAINER);
        this.element_.parentElement.insertBefore(containerIE, this.element_);
        this.element_.parentElement.removeChild(this.element_);
        containerIE.appendChild(this.element_);
      } else {
        var container = document.createElement('div');
        container.classList.add(this.CssClasses_.SLIDER_CONTAINER);
        this.element_.parentElement.insertBefore(container, this.element_);
        this.element_.parentElement.removeChild(this.element_);
        container.appendChild(this.element_);
        var backgroundFlex = document.createElement('div');
        backgroundFlex.classList.add(this.CssClasses_.BACKGROUND_FLEX);
        container.appendChild(backgroundFlex);
        this.backgroundLower_ = document.createElement('div');
        this.backgroundLower_.classList.add(this.CssClasses_.BACKGROUND_LOWER);
        backgroundFlex.appendChild(this.backgroundLower_);
        this.backgroundUpper_ = document.createElement('div');
        this.backgroundUpper_.classList.add(this.CssClasses_.BACKGROUND_UPPER);
        backgroundFlex.appendChild(this.backgroundUpper_);
      }
      this.boundInputHandler = this.onInput_.bind(this);
      this.boundChangeHandler = this.onChange_.bind(this);
      this.boundMouseUpHandler = this.onMouseUp_.bind(this);
      this.boundContainerMouseDownHandler = this.onContainerMouseDown_.bind(this);
      this.element_.addEventListener('input', this.boundInputHandler);
      this.element_.addEventListener('change', this.boundChangeHandler);
      this.element_.addEventListener('mouseup', this.boundMouseUpHandler);
      this.element_.parentElement.addEventListener('mousedown', this.boundContainerMouseDownHandler);
      this.updateValueStyles_();
      this.element_.classList.add(this.CssClasses_.IS_UPGRADED);
    }
  };
  componentHandler.register({
    constructor: MaterialSlider,
    classAsString: 'MaterialSlider',
    cssClass: 'mdl-js-slider',
    widget: true
  });
})();