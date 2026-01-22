(function() {
'use strict';
var MaterialRipple = function MaterialRipple(element) {
this.element_ = element;
this.init();
};
window['MaterialRipple'] = MaterialRipple;
MaterialRipple.prototype.Constant_ = {
INITIAL_SCALE: 'scale(0.0001, 0.0001)',
INITIAL_SIZE: '1px',
INITIAL_OPACITY: '0.4',
FINAL_OPACITY: '0',
FINAL_SCALE: ''
};
MaterialRipple.prototype.CssClasses_ = {
RIPPLE_CENTER: 'mdl-ripple--center',
RIPPLE_EFFECT_IGNORE_EVENTS: 'mdl-js-ripple-effect--ignore-events',
RIPPLE: 'mdl-ripple',
IS_ANIMATING: 'is-animating',
IS_VISIBLE: 'is-visible'
};
MaterialRipple.prototype.downHandler_ = function(event) {
if (!this.rippleElement_.style.width && !this.rippleElement_.style.height) {
var rect = this.element_.getBoundingClientRect();
this.boundHeight = rect.height;
this.boundWidth = rect.width;
this.rippleSize_ = Math.sqrt(rect.width * rect.width +
rect.height * rect.height) * 2 + 2;
this.rippleElement_.style.width = this.rippleSize_ + 'px';
this.rippleElement_.style.height = this.rippleSize_ + 'px';
}
this.rippleElement_.classList.add(this.CssClasses_.IS_VISIBLE);
if (event.type === 'mousedown' && this.ignoringMouseDown_) {
this.ignoringMouseDown_ = false;
} else {
if (event.type === 'touchstart') {
this.ignoringMouseDown_ = true;
}
var frameCount = this.getFrameCount();
if (frameCount > 0) {
return;
}
this.setFrameCount(1);
var bound = event.currentTarget.getBoundingClientRect();
var x;
var y;
if (event.clientX === 0 && event.clientY === 0) {
x = Math.round(bound.width / 2);
y = Math.round(bound.height / 2);
} else {
var clientX = event.clientX !== undefined ? event.clientX : event.touches[0].clientX;
var clientY = event.clientY !== undefined ? event.clientY : event.touches[0].clientY;
x = Math.round(clientX - bound.left);
y = Math.round(clientY - bound.top);
}
this.setRippleXY(x, y);
this.setRippleStyles(true);
window.requestAnimationFrame(this.animFrameHandler.bind(this));
}
};
MaterialRipple.prototype.upHandler_ = function(event) {
if (event && event.detail !== 2) {
window.setTimeout(function() {
this.rippleElement_.classList.remove(this.CssClasses_.IS_VISIBLE);
}.bind(this), 0);
}
};
MaterialRipple.prototype.init = function() {
if (this.element_) {
var recentering =
this.element_.classList.contains(this.CssClasses_.RIPPLE_CENTER);
if (!this.element_.classList.contains(
this.CssClasses_.RIPPLE_EFFECT_IGNORE_EVENTS)) {
this.rippleElement_ = this.element_.querySelector('.' +
this.CssClasses_.RIPPLE);
this.frameCount_ = 0;
this.rippleSize_ = 0;
this.x_ = 0;
this.y_ = 0;
this.ignoringMouseDown_ = false;
this.boundDownHandler = this.downHandler_.bind(this);
this.element_.addEventListener('mousedown',
this.boundDownHandler);
this.element_.addEventListener('touchstart',
this.boundDownHandler);
this.boundUpHandler = this.upHandler_.bind(this);
this.element_.addEventListener('mouseup', this.boundUpHandler);
this.element_.addEventListener('mouseleave', this.boundUpHandler);
this.element_.addEventListener('touchend', this.boundUpHandler);
this.element_.addEventListener('blur', this.boundUpHandler);
this.getFrameCount = function() {
return this.frameCount_;
};
this.setFrameCount = function(fC) {
this.frameCount_ = fC;
};
this.getRippleElement = function() {
return this.rippleElement_;
};
this.setRippleXY = function(newX, newY) {
this.x_ = newX;
this.y_ = newY;
};
this.setRippleStyles = function(start) {
if (this.rippleElement_ !== null) {
var transformString;
var scale;
var size;
var offset = 'translate(' + this.x_ + 'px, ' + this.y_ + 'px)';
if (start) {
scale = this.Constant_.INITIAL_SCALE;
size = this.Constant_.INITIAL_SIZE;
} else {
scale = this.Constant_.FINAL_SCALE;
size = this.rippleSize_ + 'px';
if (recentering) {
offset = 'translate(' + this.boundWidth / 2 + 'px, ' +
this.boundHeight / 2 + 'px)';
}
}
transformString = 'translate(-50%, -50%) ' + offset + scale;
this.rippleElement_.style.webkitTransform = transformString;
this.rippleElement_.style.msTransform = transformString;
this.rippleElement_.style.transform = transformString;
if (start) {
this.rippleElement_.classList.remove(this.CssClasses_.IS_ANIMATING);
} else {
this.rippleElement_.classList.add(this.CssClasses_.IS_ANIMATING);
}
}
};
this.animFrameHandler = function() {
if (this.frameCount_-- > 0) {
window.requestAnimationFrame(this.animFrameHandler.bind(this));
} else {
this.setRippleStyles(false);
}
};
}
}
};
componentHandler.register({
constructor: MaterialRipple,
classAsString: 'MaterialRipple',
cssClass: 'mdl-js-ripple-effect',
widget: false
});
})();