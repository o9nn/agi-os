import {Directive, OnInit, OnDestroy, ElementRef} from "@angular/core";
declare var $: any
@Directive({
    selector: '.ui.tooltip'
})
export class InitializeTooltip implements OnInit, OnDestroy {
    constructor(private el: ElementRef) {
    }
    public ngOnInit() {
        $(this.el.nativeElement).popup();
    }
    public ngOnDestroy() {
        $(this.el.nativeElement).popup('destroy');
    }
}