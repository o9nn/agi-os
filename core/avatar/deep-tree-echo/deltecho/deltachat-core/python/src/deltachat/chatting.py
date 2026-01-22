import os
from . import props
from .cutil import as_dc_charpointer, from_dc_charpointer, iter_array
from .capi import lib, ffi
from . import const
import attr
from attr import validators as v
from .message import Message
@attr.s
class Contact(object):
    _dc_context = attr.ib(validator=v.instance_of(ffi.CData))
    id = attr.ib(validator=v.instance_of(int))
    @property
    def _dc_contact(self):
        return ffi.gc(lib.dc_get_contact(self._dc_context, self.id), lib.dc_contact_unref)
    @props.with_doc
    def addr(self):
        return from_dc_charpointer(lib.dc_contact_get_addr(self._dc_contact))
    @props.with_doc
    def display_name(self):
        return from_dc_charpointer(lib.dc_contact_get_display_name(self._dc_contact))
    def is_blocked(self):
        return lib.dc_contact_is_blocked(self._dc_contact)
    def is_verified(self):
        return lib.dc_contact_is_verified(self._dc_contact)
@attr.s
class Chat(object):
    _dc_context = attr.ib(validator=v.instance_of(ffi.CData))
    id = attr.ib(validator=v.instance_of(int))
    @property
    def _dc_chat(self):
        return ffi.gc(lib.dc_get_chat(self._dc_context, self.id), lib.dc_chat_unref)
    def delete(self):
        lib.dc_delete_chat(self._dc_context, self.id)
    def is_deaddrop(self):
        return self.id == const.DC_CHAT_ID_DEADDROP
    def is_promoted(self):
        return not lib.dc_chat_is_unpromoted(self._dc_chat)
    def get_name(self):
        return from_dc_charpointer(lib.dc_chat_get_name(self._dc_chat))
    def set_name(self, name):
        name = as_dc_charpointer(name)
        return lib.dc_set_chat_name(self._dc_context, self.id, name)
    def get_type(self):
        return lib.dc_chat_get_type(self._dc_chat)
    def send_text(self, text):
        msg = as_dc_charpointer(text)
        msg_id = lib.dc_send_text_msg(self._dc_context, self.id, msg)
        if msg_id == 0:
            raise ValueError('message could not be send, does chat exist?')
        return Message.from_db(self._dc_context, msg_id)
    def send_file(self, path, mime_type='application/octet-stream'):
        path = as_dc_charpointer(path)
        mtype = as_dc_charpointer(mime_type)
        msg = Message.new(self._dc_context, 'file')
        msg.set_file(path, mtype)
        msg_id = lib.dc_send_msg(self._dc_context, self.id, msg._dc_msg)
        if msg_id == 0:
            raise ValueError('message could not be send, does chat exist?')
        return Message.from_db(self._dc_context, msg_id)
    def send_image(self, path):
        if not os.path.exists(path):
            raise ValueError('path does not exist: {!r}'.format(path))
        msg = Message.new(self._dc_context, 'image')
        msg.set_file(path)
        msg_id = lib.dc_send_msg(self._dc_context, self.id, msg._dc_msg)
        return Message.from_db(self._dc_context, msg_id)
    def prepare_file(self, path, mime_type=None, view_type='file'):
        path = as_dc_charpointer(path)
        mtype = as_dc_charpointer(mime_type)
        msg = Message.new(self._dc_context, view_type)
        msg.set_file(path, mtype)
        msg_id = lib.dc_prepare_msg(self._dc_context, self.id, msg._dc_msg)
        if msg_id == 0:
            raise ValueError('message could not be prepared, does chat exist?')
        return Message.from_db(self._dc_context, msg_id)
    def send_prepared(self, message):
        msg_id = lib.dc_send_msg(self._dc_context, 0, message._dc_msg)
        if msg_id == 0:
            raise ValueError('message could not be sent')
        return Message.from_db(self._dc_context, msg_id)
    def get_messages(self):
        dc_array = ffi.gc(lib.dc_get_chat_msgs(self._dc_context, self.id, 0, 0), lib.dc_array_unref)
        return list(iter_array(dc_array, lambda x: Message.from_db(self._dc_context, x)))
    def count_fresh_messages(self):
        return lib.dc_get_fresh_msg_cnt(self._dc_context, self.id)
    def mark_noticed(self):
        return lib.dc_marknoticed_chat(self._dc_context, self.id)
    def add_contact(self, contact):
        ret = lib.dc_add_contact_to_chat(self._dc_context, self.id, contact.id)
        if ret != 1:
            raise ValueError('could not add contact {!r} to chat'.format(contact))
    def remove_contact(self, contact):
        ret = lib.dc_remove_contact_from_chat(self._dc_context, self.id, contact.id)
        if ret != 1:
            raise ValueError('could not remove contact {!r} from chat'.format(contact))
    def get_contacts(self):
        dc_array = ffi.gc(lib.dc_get_chat_contacts(self._dc_context, self.id), lib.dc_array_unref)
        return list(iter_array(dc_array, lambda id: Contact(self._dc_context, id)))