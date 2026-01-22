 (function(modules) { 
 	var installedModules = {};
 	function __webpack_require__(moduleId) {
 		if(installedModules[moduleId])
 			return installedModules[moduleId].exports;
 		var module = installedModules[moduleId] = {
 			exports: {},
 			id: moduleId,
 			loaded: false
 		};
 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
 		module.loaded = true;
 		return module.exports;
 	}
 	__webpack_require__.m = modules;
 	__webpack_require__.c = installedModules;
 	__webpack_require__.p = "";
 	return __webpack_require__(0);
 })
 ([
 function(module, exports) {
	'use strict';
	(function () {
	  'use strict';
	  function MaterialStepper(element) {
	    this.element_ = element;
	    this.init();
	  }
	  window.MaterialStepper = MaterialStepper;
	  MaterialStepper.prototype.Stepper_ = {};
	  MaterialStepper.prototype.getStepper_ = function () {
	    return {
	      isLinear: this.element_.classList.contains(this.CssClasses_.STEPPER_LINEAR),
	      hasFeedback: this.element_.classList.contains(this.CssClasses_.STEPPER_FEEDBACK)
	    };
	  };
	  MaterialStepper.prototype.StepState_ = {
	    COMPLETED: 'completed',
	    ERROR: 'error',
	    NORMAL: 'normal'
	  };
	  MaterialStepper.prototype.DatasetAttributes_ = {
	    CONTINUE: 'stepper-next',
	    CANCEL: 'stepper-cancel',
	    SKIP: 'stepper-skip',
	    BACK: 'stepper-back'
	  };
	  MaterialStepper.prototype.defineCustomEvent = function (evtName, bubble, cancel) {
	    var ev;
	    if ('CustomEvent' in window && typeof window.CustomEvent === 'function') {
	      ev = new Event(evtName, {
	        bubbles: bubble,
	        cancelable: cancel
	      });
	    } else {
	      ev = document.createEvent('Events');
	      ev.initEvent(evtName, bubble, cancel);
	    }
	    return ev;
	  };
	  MaterialStepper.prototype.CustomEvents_ = {
	    onstepnext: MaterialStepper.prototype.defineCustomEvent('onstepnext', true, true),
	    onstepcancel: MaterialStepper.prototype.defineCustomEvent('onstepcancel', true, true),
	    onstepskip: MaterialStepper.prototype.defineCustomEvent('onstepskip', true, true),
	    onstepback: MaterialStepper.prototype.defineCustomEvent('onstepback', true, true),
	    onstepcomplete: MaterialStepper.prototype.defineCustomEvent('onstepcomplete', true, true),
	    onsteperror: MaterialStepper.prototype.defineCustomEvent('onsteperror', true, true),
	    onsteppercomplete: MaterialStepper.prototype.defineCustomEvent('onsteppercomplete', true, true)
	  };
	  MaterialStepper.prototype.CssClasses_ = {
	    BUTTON_JS: 'mdl-js-button',
	    STEPPER_LINEAR: 'mdl-stepper--linear',
	    STEPPER_FEEDBACK: 'mdl-stepper--feedback',
	    STEP_COMPLETED: 'mdl-step--completed',
	    STEP_ERROR: 'mdl-step--error',
	    STEP_TRANSIENT: 'mdl-step--transient',
	    STEP_OPTIONAL: 'mdl-step--optional',
	    STEP_EDITABLE: 'mdl-step--editable',
	    IS_ACTIVE: 'is-active',
	    TRANSIENT: 'mdl-step__transient',
	    TRANSIENT_OVERLAY: 'mdl-step__transient-overlay',
	    TRANSIENT_LOADER: 'mdl-step__transient-loader',
	    SPINNER: 'mdl-spinner',
	    SPINNER_JS: 'mdl-js-spinner',
	    SPINNER_IS_ACTIVE: 'is-active',
	    STEPPER: 'mdl-stepper',
	    STEP: 'mdl-step',
	    STEP_LABEL: 'mdl-step__label',
	    STEP_LABEL_INDICATOR: 'mdl-step__label-indicator',
	    STEP_LABEL_INDICATOR_CONTENT: 'mdl-step__label-indicator-content',
	    STEP_TITLE: 'mdl-step__title',
	    STEP_TITLE_TEXT: 'mdl-step__title-text',
	    STEP_TITLE_MESSAGE: 'mdl-step__title-message',
	    STEP_CONTENT: 'mdl-step__content',
	    STEP_ACTIONS: 'mdl-step__actions'
	  };
	  MaterialStepper.prototype.Steps_ = {};
	  MaterialStepper.prototype.getIndicatorElement_ = function (step) {
	    var indicatorElement;
	    var indicatorContent;
	    indicatorElement = document.createElement('span');
	    indicatorContent = this.getIndicatorContentNormal_(step.labelndicatorText);
	    indicatorElement.classList.add(this.CssClasses_.STEP_LABEL_INDICATOR);
	    indicatorElement.appendChild(indicatorContent);
	    return indicatorElement;
	  };
	  MaterialStepper.prototype.getIndicatorContentNormal_ = function (text) {
	    var normal;
	    normal = document.createElement('span');
	    normal.classList.add(this.CssClasses_.STEP_LABEL_INDICATOR_CONTENT);
	    normal.textContent = text;
	    return normal;
	  };
	  MaterialStepper.prototype.getIndicatorContentCompleted_ = function (isEditable) {
	    var completed;
	    completed = document.createElement('i');
	    completed.classList.add('material-icons', this.CssClasses_.STEP_LABEL_INDICATOR_CONTENT);
	    completed.textContent = isEditable ? 'edit' : 'check';
	    return completed;
	  };
	  MaterialStepper.prototype.getIndicatorContentError_ = function () {
	    var error;
	    error = document.createElement('span');
	    error.classList.add(this.CssClasses_.STEP_LABEL_INDICATOR_CONTENT);
	    error.textContent = '!';
	    return error;
	  };
	  MaterialStepper.prototype.getStepModel_ = function (step, id) {
	    var model;
	    var selectorActionsBack;
	    var selectorActionsCancel;
	    var selectorActionsNext;
	    var selectorActionsSkip;
	    selectorActionsBack = '[data-' + this.DatasetAttributes_.BACK + ']';
	    selectorActionsCancel = '[data-' + this.DatasetAttributes_.CANCEL + ']';
	    selectorActionsNext = '[data-' + this.DatasetAttributes_.CONTINUE + ']';
	    selectorActionsSkip = '[data-' + this.DatasetAttributes_.SKIP + ']';
	    model = {};
	    model.container = step;
	    model.id = id;
	    model.label = step.querySelector('.' + this.CssClasses_.STEP_LABEL);
	    model.labelndicatorText = id;
	    model.labelTitle = step.querySelector('.' + this.CssClasses_.STEP_TITLE);
	    model.labelTitleText = step.querySelector('.' + this.CssClasses_.STEP_TITLE_TEXT).textContent;
	    model.labelTitleMessage = step.querySelector('.' + this.CssClasses_.STEP_TITLE_MESSAGE);
	    model.labelTitleMessageText = model.labelTitleMessage ? model.labelTitleMessage.textContent : '';
	    model.content = step.querySelector('.' + this.CssClasses_.STEP_CONTENT);
	    model.actions = step.querySelector('.' + this.CssClasses_.STEP_ACTIONS);
	    model.actionsBack = model.actions.querySelector(selectorActionsBack) || null;
	    model.actionsCancel = model.actions.querySelector(selectorActionsCancel) || null;
	    model.actionsNext = model.actions.querySelector(selectorActionsNext) || null;
	    model.actionsSkip = model.actions.querySelector(selectorActionsSkip) || null;
	    model.labelIndicator = model.label.querySelector('.' + this.CssClasses_.STEP_LABEL_INDICATOR);
	    if (!model.labelIndicator) {
	      model.labelIndicator = this.getIndicatorElement_(model);
	      model.label.appendChild(model.labelIndicator);
	    }
	    if (step.classList.contains(this.CssClasses_.STEP_COMPLETED)) {
	      model.state = this.StepState_.COMPLETED;
	    } else if (step.classList.contains(this.CssClasses_.STEP_ERROR)) {
	      model.state = this.StepState_.ERROR;
	    } else {
	      model.state = this.StepState_.NORMAL;
	    }
	    model.isActive = step.classList.contains(this.CssClasses_.IS_ACTIVE);
	    model.isOptional = step.classList.contains(this.CssClasses_.STEP_OPTIONAL);
	    model.isEditable = step.classList.contains(this.CssClasses_.STEP_EDITABLE);
	    return model;
	  };
	  MaterialStepper.prototype.getActive = function () {
	    return this.Steps_.collection[this.Steps_.active - 1].container;
	  };
	  MaterialStepper.prototype.getActiveId = function () {
	    return this.Steps_.collection[this.Steps_.active - 1].id;
	  };
	  MaterialStepper.prototype.getSteps_ = function () {
	    var collection;
	    var total;
	    var completed;
	    var optional;
	    var active;
	    var stepElements;
	    var i;
	    collection = [];
	    total = 0;
	    completed = 0;
	    optional = 0;
	    active = 0;
	    stepElements = this.element_.querySelectorAll('.' + this.CssClasses_.STEP);
	    for (i = 0; i < stepElements.length; i++) {
	      collection[i] = this.getStepModel_(stepElements[i], i + 1);
	      if (collection[i].isOptional) {
	        optional += 1;
	      }
	      if (collection[i].isActive) {
	        active = collection[i].id;
	      }
	      stepElements[i].addEventListener('scroll', function (event) {
	        event.target.scrollTop = 0;
	      });
	    }
	    total = collection.length;
	    return {
	      collection: collection,
	      total: total,
	      completed: completed,
	      optional: optional,
	      active: active
	    };
	  };
	  MaterialStepper.prototype.setStepActive_ = function (step) {
	    var stepsDeactivator;
	    if (this.hasTransient()) return false;
	    stepsDeactivator = function stepsDeactivator(step) {
	      step.container.classList.remove(this.CssClasses_.IS_ACTIVE);
	      if (step.isActive) {
	        step.isActive = false;
	      }
	    };
	    this.Steps_.collection.forEach(stepsDeactivator.bind(this));
	    step.container.classList.remove(this.CssClasses_.STEP_TRANSIENT);
	    step.container.classList.add(this.CssClasses_.IS_ACTIVE);
	    step.isActive = true;
	    this.Steps_.active = step.id;
	    return true;
	  };
	  MaterialStepper.prototype.setActive_ = function (id) {
	    var active;
	    var first;
	    var i;
	    var moved;
	    var step;
	    if (!isNaN(id) && (id > this.Steps_.total || id <= 0)) return false;
	    moved = false;
	    if (id) {
	      for (i = 0; i < this.Steps_.total; i++) {
	        step = this.Steps_.collection[i];
	        if (step.id === id) {
	          moved = this.setStepActive_(step);
	          break;
	        }
	      }
	    } else {
	      active = this.element_.querySelector('.' + this.CssClasses_.IS_ACTIVE);
	      if (!active) {
	        first = this.Steps_.collection[0];
	        moved = this.setStepActive_(first);
	      }
	    }
	    if (this.Stepper_.isLinear) {
	      this.updateLinearStates_();
	    }
	    return moved;
	  };
	  MaterialStepper.prototype.updateStepState_ = function (step, state) {
	    var stateClass;
	    var indicatorContent;
	    var currentIndicatorContent;
	    var stepperCompleted;
	    var hasRequired;
	    var stepItem;
	    var item;
	    var selectorIndicator;
	    selectorIndicator = '.' + this.CssClasses_.STEP_LABEL_INDICATOR_CONTENT;
	    if (step.state === state) return false;
	    if (step.state === this.StepState_.COMPLETED) {
	      this.Steps_.completed -= 1;
	    }
	    currentIndicatorContent = step.labelIndicator.querySelector(selectorIndicator);
	    switch (state) {
	      case this.StepState_.COMPLETED:
	        {
	          this.Steps_.completed += 1;
	          step.container.classList.remove(this.CssClasses_.STEP_ERROR);
	          indicatorContent = this.getIndicatorContentCompleted_(step.isEditable);
	          stateClass = this.CssClasses_.STEP_COMPLETED;
	          break;
	        }
	      case this.StepState_.ERROR:
	        {
	          step.container.classList.remove(this.CssClasses_.STEP_COMPLETED);
	          indicatorContent = this.getIndicatorContentError_();
	          stateClass = this.CssClasses_.STEP_ERROR;
	          break;
	        }
	      case this.StepState_.NORMAL:
	        {
	          step.container.classList.remove(this.CssClasses_.STEP_COMPLETED);
	          step.container.classList.remove(this.CssClasses_.STEP_ERROR);
	          indicatorContent = this.getIndicatorContentNormal_(step.labelndicatorText);
	          break;
	        }
	      default:
	        {
	          break;
	        }
	    }
	    if (stateClass) {
	      step.container.classList.add(stateClass);
	    }
	    step.labelIndicator.replaceChild(indicatorContent, currentIndicatorContent);
	    step.state = state;
	    stepperCompleted = false;
	    if (this.Steps_.completed === this.Steps_.total) {
	      stepperCompleted = true;
	    } else if (this.Steps_.completed === this.Steps_.total - this.Steps_.optional) {
	      for (item in this.Steps_.collection) {
	        if (this.Steps_.collection.hasOwnProperty(item)) {
	          stepItem = this.Steps_.collection[item];
	          hasRequired = !stepItem.isOptional && stepItem.state !== this.StepState_.COMPLETED;
	          if (hasRequired) break;
	        }
	      }
	      stepperCompleted = !hasRequired;
	    }
	    if (stepperCompleted) {
	      this.dispatchEventOnStepperComplete_();
	    }
	    return true;
	  };
	  MaterialStepper.prototype.updateLinearStates_ = function () {
	    var i;
	    for (i = 0; i < this.Steps_.total; i++) {
	      if (this.Steps_.collection[i].isActive) {
	        break;
	      } else {
	        if (this.Steps_.collection[i].isOptional) continue;
	        this.updateStepState_(this.Steps_.collection[i], this.StepState_.COMPLETED);
	      }
	    }
	  };
	  MaterialStepper.prototype.back = function () {
	    var moved;
	    var moveStep;
	    var model;
	    var step;
	    var previous;
	    moved = false;
	    moveStep = function moveStep(step) {
	      var stepActivated;
	      stepActivated = this.setActive_(step.id);
	      if (stepActivated) {
	        if (stepActivated && this.Stepper_.hasFeedback) {
	          this.removeTransientEffect_(step);
	        }
	      }
	      return stepActivated;
	    };
	    for (model in this.Steps_.collection) {
	      if (this.Steps_.collection.hasOwnProperty(model)) {
	        step = this.Steps_.collection[model];
	        if (step.isActive) {
	          previous = this.Steps_.collection[step.id - 2];
	          if (!previous) return false;
	          if (this.Stepper_.isLinear) {
	            if (previous.isEditable) {
	              moved = moveStep.bind(this)(previous);
	            }
	          } else {
	            moved = moveStep.bind(this)(previous);
	          }
	          break;
	        }
	      }
	    }
	    return moved;
	  };
	  MaterialStepper.prototype.skip = function () {
	    var moved;
	    var model;
	    var step;
	    moved = false;
	    for (model in this.Steps_.collection) {
	      if (this.Steps_.collection.hasOwnProperty(model)) {
	        step = this.Steps_.collection[model];
	        if (step.isActive) {
	          if (step.isOptional) {
	            moved = this.setActive_(step.id + 1);
	            if (moved && this.Stepper_.hasFeedback) {
	              this.removeTransientEffect_(step);
	            }
	          }
	          break;
	        }
	      }
	    }
	    return moved;
	  };
	  MaterialStepper.prototype.goto = function (id) {
	    return this.setActive_(id);
	  };
	  MaterialStepper.prototype.error = function (message) {
	    var model;
	    var step;
	    for (model in this.Steps_.collection) {
	      if (this.Steps_.collection.hasOwnProperty(model)) {
	        step = this.Steps_.collection[model];
	        if (step.isActive) {
	          if (this.Stepper_.hasFeedback) {
	            this.removeTransientEffect_(step);
	          }
	          this.updateStepState_(step, this.StepState_.ERROR);
	          if (message) {
	            this.updateTitleMessage_(step, message);
	          }
	          this.dispatchEventOnStepError_(step);
	          break;
	        }
	      }
	    }
	  };
	  MaterialStepper.prototype.next = function () {
	    var moved;
	    var step;
	    var activate;
	    var model;
	    var item;
	    var stepItem;
	    moved = false;
	    for (model in this.Steps_.collection) {
	      if (this.Steps_.collection.hasOwnProperty(model)) {
	        step = this.Steps_.collection[model];
	        if (step.isActive) {
	          activate = step.id + 1;
	          if (this.Stepper_.hasFeedback) {
	            this.removeTransientEffect_(step);
	          }
	          if (step.state === this.StepState_.ERROR) {
	            if (step.labelTitleMessageText) {
	              this.updateTitleMessage_(step, step.labelTitleMessageText);
	            } else {
	              this.removeTitleMessage_(step);
	            }
	          }
	          if (step.isEditable && this.Stepper_.isLinear) {
	            for (item in this.Steps_.collection) {
	              if (this.Steps_.collection.hasOwnProperty(item)) {
	                stepItem = this.Steps_.collection[item];
	                if (stepItem.id > step.id && stepItem.state !== this.StepState_.COMPLETED) {
	                  activate = stepItem.id;
	                  break;
	                }
	              }
	            }
	          }
	          moved = this.setActive_(activate);
	          if (this.Stepper_.isLinear) {
	            if (step.isOptional || step.id === this.Steps_.total) {
	              this.updateStepState_(step, this.StepState_.COMPLETED);
	            }
	          } else {
	            this.updateStepState_(step, this.StepState_.COMPLETED);
	          }
	          this.dispatchEventOnStepComplete_(step);
	          break;
	        }
	      }
	    }
	    return moved;
	  };
	  MaterialStepper.prototype.updateTitleMessage_ = function (step, text) {
	    var titleMessage;
	    titleMessage = step.container.querySelector('.' + this.CssClasses_.STEP_TITLE_MESSAGE);
	    if (!titleMessage) {
	      titleMessage = document.createElement('span');
	      titleMessage.classList.add(this.CssClasses_.STEP_TITLE_MESSAGE);
	      step.labelTitle.appendChild(titleMessage);
	    }
	    titleMessage.textContent = text;
	  };
	  MaterialStepper.prototype.removeTitleMessage_ = function (step) {
	    var titleMessage;
	    titleMessage = step.container.querySelector('.' + this.CssClasses_.STEP_TITLE_MESSAGE);
	    if (titleMessage) {
	      titleMessage.parentNode.removeChild(titleMessage);
	    }
	  };
	  MaterialStepper.prototype.removeTransientEffect_ = function (step) {
	    var transient;
	    transient = step.content.querySelector('.' + this.CssClasses_.TRANSIENT);
	    if (!transient) return false;
	    step.container.classList.remove(this.CssClasses_.STEP_TRANSIENT);
	    step.content.removeChild(transient);
	    return true;
	  };
	  MaterialStepper.prototype.addTransientEffect_ = function (step) {
	    var transient;
	    var overlay;
	    var loader;
	    var spinner;
	    if (step.content.querySelector('.' + this.CssClasses_.TRANSIENT)) return false;
	    transient = document.createElement('div');
	    overlay = document.createElement('div');
	    loader = document.createElement('div');
	    spinner = document.createElement('div');
	    transient.classList.add(this.CssClasses_.TRANSIENT);
	    overlay.classList.add(this.CssClasses_.TRANSIENT_OVERLAY);
	    loader.classList.add(this.CssClasses_.TRANSIENT_LOADER);
	    spinner.classList.add(this.CssClasses_.SPINNER);
	    spinner.classList.add(this.CssClasses_.SPINNER_JS);
	    spinner.classList.add(this.CssClasses_.SPINNER_IS_ACTIVE);
	    loader.appendChild(spinner);
	    transient.appendChild(overlay);
	    transient.appendChild(loader);
	    step.container.classList.add(this.CssClasses_.STEP_TRANSIENT);
	    step.content.appendChild(transient);
	    componentHandler.upgradeDom();
	    return true;
	  };
	  MaterialStepper.prototype.setCustomEvents_ = function () {
	    var linearLabels;
	    var nonLinearLabels;
	    var dispatchCustomEvents;
	    linearLabels = function linearLabels(step) {
	      if (step.isEditable) {
	        step.label.addEventListener('click', function (event) {
	          event.preventDefault();
	          if (step.state === this.StepState_.COMPLETED) {
	            this.setStepActive_(step);
	          }
	        }.bind(this));
	      }
	    };
	    nonLinearLabels = function nonLinearLabels(step) {
	      step.label.addEventListener('click', function (event) {
	        event.preventDefault();
	        this.setStepActive_(step);
	      }.bind(this));
	    };
	    dispatchCustomEvents = function dispatchCustomEvents(step) {
	      this.dispatchEventOnStepNext_(step);
	      this.dispatchEventOnStepCancel_(step);
	      this.dispatchEventOnStepSkip_(step);
	      this.dispatchEventOnStepBack_(step);
	    };
	    if (this.Stepper_.isLinear) {
	      this.Steps_.collection.forEach(linearLabels.bind(this));
	    } else {
	      this.Steps_.collection.forEach(nonLinearLabels.bind(this));
	    }
	    this.Steps_.collection.forEach(dispatchCustomEvents.bind(this));
	  };
	  MaterialStepper.prototype.dispatchEventOnStepComplete_ = function (step) {
	    step.container.dispatchEvent(this.CustomEvents_.onstepcomplete);
	  };
	  MaterialStepper.prototype.dispatchEventOnStepError_ = function (step) {
	    step.container.dispatchEvent(this.CustomEvents_.onsteperror);
	  };
	  MaterialStepper.prototype.dispatchEventOnStepperComplete_ = function () {
	    this.element_.dispatchEvent(this.CustomEvents_.onsteppercomplete);
	  };
	  MaterialStepper.prototype.dispatchEventOnStepNext_ = function (step) {
	    if (!step.actionsNext) return false;
	    step.actionsNext.addEventListener('click', function () {
	      if (this.Stepper_.hasFeedback) {
	        this.addTransientEffect_(step);
	      }
	      step.container.dispatchEvent(this.CustomEvents_.onstepnext);
	    }.bind(this));
	    return true;
	  };
	  MaterialStepper.prototype.dispatchEventOnStepCancel_ = function (step) {
	    if (!step.actionsCancel) return false;
	    step.actionsCancel.addEventListener('click', function (event) {
	      event.preventDefault();
	      step.container.dispatchEvent(this.CustomEvents_.onstepcancel);
	    }.bind(this));
	    return true;
	  };
	  MaterialStepper.prototype.dispatchEventOnStepSkip_ = function (step) {
	    if (!step.actionsSkip) return false;
	    step.actionsSkip.addEventListener('click', function (event) {
	      event.preventDefault();
	      step.container.dispatchEvent(this.CustomEvents_.onstepskip);
	    }.bind(this));
	    return true;
	  };
	  MaterialStepper.prototype.dispatchEventOnStepBack_ = function (step) {
	    if (!step.actionsBack) return false;
	    step.actionsBack.addEventListener('click', function (event) {
	      event.preventDefault();
	      step.container.dispatchEvent(this.CustomEvents_.onstepback);
	    }.bind(this));
	    return true;
	  };
	  MaterialStepper.prototype.hasTransient = function () {
	    var cssClasseStep;
	    var cssClasseStepContent;
	    var cssClasseTransient;
	    var selectorTransient;
	    var transient;
	    cssClasseStep = '.' + this.CssClasses_.STEP;
	    cssClasseStepContent = '.' + this.CssClasses_.STEP_CONTENT;
	    cssClasseTransient = '.' + this.CssClasses_.TRANSIENT;
	    selectorTransient = cssClasseStep + ' > ' + cssClasseStepContent + ' > ' + cssClasseTransient;
	    transient = this.element_.querySelector(selectorTransient);
	    return transient !== null;
	  };
	  MaterialStepper.prototype.init = function () {
	    if (this.element_) {
	      this.Stepper_ = this.getStepper_();
	      this.Steps_ = this.getSteps_();
	      this.setActive_();
	      this.setCustomEvents_();
	    }
	  };
	  componentHandler.register({
	    constructor: MaterialStepper,
	    classAsString: 'MaterialStepper',
	    cssClass: 'mdl-stepper',
	    widget: true
	  });
	})();
 }
 ]);