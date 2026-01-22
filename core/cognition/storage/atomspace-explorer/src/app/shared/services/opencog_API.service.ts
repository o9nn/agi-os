import { Injectable } from '@angular/core';
import { Http, Response } from '@angular/http';
import 'rxjs/add/operator/map';
import { Observable } from 'rxjs/Observable';
import { configs } from '../../app.config';
@Injectable()
export class OpencogAPIService {
  private atomspace_api = configs.atomspace_api;
  constructor(private http: Http) {
  }
  private getJson(res: Response) {
    return res.json();
  }
  getAtoms(url) {
    if (url.endsWith('.json')) {
      return this.http.get(`${url}`).map(this.getJson);
    } else {
      return this.http.get(`${url}${this.atomspace_api}/atoms`).map(this.getJson);
    }
  }
  getUnorderedLinkTypes(url) {
    return this.executeScmCommand(url, '(cog-get-all-subtypes \'UnorderedLink)');
  }
  executeScmCommand(url, command) {
    const body = { 'command': command };
    return this.http.post(`${url}${this.atomspace_api}/scheme`, body)
      .map(this.getJson);
  }
}