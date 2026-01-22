import {Component, EventEmitter, Input, Output, AfterViewInit} from '@angular/core';
declare var $: any
@Component({
    selector:'confirm-dialog',
    templateUrl:'confirmation-dialog.template.html'
})
export class ConfirmationDialogComponent  implements AfterViewInit{
    ngAfterViewInit(): void {
    }
}