import { Pipe, PipeTransform } from '@angular/core';
import { TranslateService } from './translate.service';
@Pipe({
name: 'translate',
pure: false
})
export class TranslatePipe implements PipeTransform {
constructor(private _translateService: TranslateService) { }
transform(value: string, args: any[]): any {
if (!value) return;
return this._translateService.instant(value);
}
}