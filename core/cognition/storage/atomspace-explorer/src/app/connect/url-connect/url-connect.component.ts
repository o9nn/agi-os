import { Component, OnInit, Inject } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { UrlConnectService } from './url-connect.service';
import { OpencogAPIService } from './../../shared/services/opencog_API.service';
import { AtomService, AtomServiceData } from 'ng2-atomspace-visualizer';
import { Router } from '@angular/router';
import { LocalStorageService } from 'angular-2-local-storage';
import { configs } from '../../app.config';
import { TranslateConfig } from '../../core/translate/translate-config';
import { TranslateService } from '../../core/translate/translate.service';
const defUnorderedLinktypes = [
'AbsentLink', 'AndLink', 'EqualLink',
'EquivalenceLink', 'IdenticalLink',
'NotLink', 'OrLink', 'SetLink', 'SimilarityLink'];
let flag = false;
let numberAtoms = 0;
@Component({
selector: 'app-url-connect',
templateUrl: './url-connect.component.html',
styleUrls: ['./url-connect.component.css']
})
export class UrlConnectComponent implements OnInit {
private urlKey = 'ase-fetch-url';
private maxAtoms = 2500;
private subscription = null;
private unorderedLinkTypesArr: string[] = defUnorderedLinktypes;
public form: FormGroup;
public url: string;
public errMsg = '';
public connecting = false;
private fileJSON = 'assets/' + configs.sample_data_file;
constructor(@Inject(FormBuilder) fb: FormBuilder,
private service: UrlConnectService,
private cogAPIService: OpencogAPIService,
private atomsService: AtomService,
private router: Router,
private translate: TranslateService,
private localStorageService: LocalStorageService) {
this.form = fb.group({
url: ''
});
}
ngOnInit() {
const savedURL: string = this.localStorageService.get(this.urlKey);
if (savedURL !== null) {
this.url = savedURL;
}
setInterval(() => {
this.update();
}, 3000);
console.log('setInterval called');
}
update() {
if(flag){
this.fetchJson();
}
}
fetchJson() {
console.log('\n' + 'Fetching from ' + this.url);
if (!this.url) {
this.errMsg = 'Invalid URL';
return;
}
if (this.url.endsWith('.json')) {
} else {
this.url = this.url.replace(/(\/$)/, '');
this.fetchLinkTypes(this.url);
}
this.errMsg = '';
this.connecting = true;
this.subscription = this.service.get(this.url)
.subscribe(res => {
const numAtoms = res.result.atoms.length;
numberAtoms = res.result.atoms.length;
console.log('numberAtoms in fetchJson =',numberAtoms);
for (var i = 0; i < numAtoms; i++){
res.result.atoms[i]['attentionvalue'] =  {"lti": 0, "sti": 0, "vlti": false};
}
console.log('Fetched ' + numAtoms + ' atoms from ' + this.url);
this.localStorageService.set(this.urlKey, this.url);
if (numAtoms > this.maxAtoms) {
this.errMsg = 'Fetched Atoms count (' + numAtoms + ') exceeds currently supported maximum (' + this.maxAtoms + ').';
return;
}
this.visualizeResult(res);
console.log('res\n',res);
}, err => {
this.connecting = false;
this.errMsg = err.message;
console.log(err);
});
flag = true;
}
fetchSampleJson() {
console.log('\n' + 'Loading sample data from file ' + this.fileJSON);
this.errMsg = '';
this.connecting = true;
this.service.get(this.fileJSON)
.subscribe(res => {
const numAtoms = res.result.atoms.length;
console.log('Fetched ' + numAtoms + ' atoms from ' + this.fileJSON);
if (numAtoms > this.maxAtoms) {
this.errMsg = 'Fetched Atoms count (' + numAtoms + ') exceeds currently supported maximum (' + this.maxAtoms + ').';
return;
}
this.visualizeResult(res);
}, err => {
this.connecting = false;
this.errMsg = err.message;
console.log(err);
});
}
fetchLinkTypes(url: string) {
this.errMsg = '';
this.connecting = true;
this.subscription = this.cogAPIService.getUnorderedLinkTypes(url)
.subscribe(res => {
const unorderedLinkTypes: string = res.response;
const unorderedLinkTypesTrimmed: string = unorderedLinkTypes.trim().slice(1, -1);
this.unorderedLinkTypesArr = unorderedLinkTypesTrimmed.split(' ').sort();
console.log('Fetched ' + this.unorderedLinkTypesArr.length + ' unordered link types from ' + url);
}, err => {
console.log(err);
if (err.status === 500) {
console.log('cog-get-all-subtypes command not supported. Unable to fetching unordered link types');
}
this.errMsg = err.message;
this.connecting = false;
});
}
private visualizeResult(res) {
const as_data: AtomServiceData = { atoms: null, unordered_linktypes: null, custom_style: null, language: null, numAtoms: null};
as_data.atoms = res;
as_data.numAtoms = numberAtoms;
console.log("numberAtoms in visualizeResult =", numberAtoms);
if (this.unorderedLinkTypesArr !== null) {
as_data.unordered_linktypes = this.unorderedLinkTypesArr;
}
as_data.language = this.translate.currentLang;
this.atomsService.changeItem(as_data);
this.router.navigate(['cog-visualizer']);
}
public reset() {
this.connecting = false;
this.errMsg = '';
this.url = '';
this.localStorageService.remove(this.urlKey);
if (this.subscription) {
this.subscription.unsubscribe();
this.subscription = null;
}
}
public cancel() {
this.connecting = false;
this.errMsg = '';
this.router.navigate(['']);
}
}