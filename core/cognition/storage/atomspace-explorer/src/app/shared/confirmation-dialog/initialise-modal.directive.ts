import {Directive, OnInit, OnDestroy, ElementRef} from "@angular/core";
declare var $: any
@Directive({
    selector: '.ui.modal'
})
export class InitializeModal implements OnInit, OnDestroy {
    constructor(private el: ElementRef) {
    }
    public ngOnInit() {
    }
    public ngOnDestroy() {
        $(this.el.nativeElement).modal('destroy');
    }
}