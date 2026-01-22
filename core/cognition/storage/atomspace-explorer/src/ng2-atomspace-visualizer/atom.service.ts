import { Injectable } from '@angular/core';
import { BehaviorSubject } from 'rxjs/BehaviorSubject';
import 'rxjs/add/operator/distinctUntilChanged';
const emptyAtomSpace = { 'result': { 'atoms': [] }};
const emptyUnorderedLinktypes = [];
const emptyStyle = '';
const defaultLang = 'en';
export interface AtomServiceData {
atoms: object;
unordered_linktypes: string[];
custom_style: string;
language: string;
numAtoms: number;
}
@Injectable()
export class AtomService {
private defaultState: AtomServiceData = {
atoms: emptyAtomSpace,
unordered_linktypes: emptyUnorderedLinktypes,
custom_style: emptyStyle,
language: defaultLang,
numAtoms: 0};
private editItemSource: BehaviorSubject<any> = new BehaviorSubject(this.defaultState);
public editItem = this.editItemSource.asObservable().distinctUntilChanged();
changeItem(state) {
this.editItemSource.next(state);
}
}