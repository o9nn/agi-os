import {Directive, OnInit, OnDestroy, ElementRef} from "@angular/core";
declare var $: any
@Directive({
    selector: '.card'
})
export class CardTransition implements OnInit, OnDestroy {
    constructor(private el: ElementRef) {
    }
    public ngOnInit() {
        $(this.el.nativeElement).transition({
            animation : 'pulse',
            reverse   : true,
            interval  : 200
        });
    }
    public ngOnDestroy() {
    }
}