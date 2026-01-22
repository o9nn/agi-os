import os
from . import props
from .cutil import from_dc_charpointer, as_dc_charpointer
from .capi import lib, ffi
from . import const
from datetime import datetime
import attr
from attr import validators as v
@attr.s
class Message(object):
    _dc_context = attr.ib(validator=v.instance_of(ffi.CData))
    try:
        id = attr.ib(validator=v.instance_of((int, long)))
    except NameError:
        id = attr.ib(validator=v.instance_of(int))
    @property
    def _dc_msg(self):
        if self.id > 0:
            return ffi.gc(lib.dc_get_msg(self._dc_context, self.id), lib.dc_msg_unref)
        return self._dc_msg_volatile
    @classmethod
    def from_db(cls, _dc_context, id):
        assert id > 0
        return cls(_dc_context, id)
    @classmethod
    def new(cls, dc_context, view_type):
        msg = cls(dc_context, 0)
        view_type_code = MessageType.get_typecode(view_type)
        msg._dc_msg_volatile = ffi.gc(lib.dc_msg_new(dc_context, view_type_code), lib.dc_msg_unref)
        return msg
    def get_state(self):
        return MessageState(self)
    @props.with_doc
    def text(self):
        return from_dc_charpointer(lib.dc_msg_get_text(self._dc_msg))
    def set_text(self, text):
        return lib.dc_msg_set_text(self._dc_msg, as_dc_charpointer(text))
    @props.with_doc
    def filename(self):
        return from_dc_charpointer(lib.dc_msg_get_file(self._dc_msg))
    def set_file(self, path, mime_type=None):
        mtype = ffi.NULL if mime_type is None else mime_type
        assert os.path.exists(path)
        lib.dc_msg_set_file(self._dc_msg, as_dc_charpointer(path), mtype)
    @props.with_doc
    def basename(self):
        return from_dc_charpointer(lib.dc_msg_get_filename(self._dc_msg))
    @props.with_doc
    def filemime(self):
        return from_dc_charpointer(lib.dc_msg_get_filemime(self._dc_msg))
    @props.with_doc
    def view_type(self):
        return MessageType(lib.dc_msg_get_viewtype(self._dc_msg))
    @props.with_doc
    def time_sent(self):
        ts = lib.dc_msg_get_timestamp(self._dc_msg)
        return datetime.utcfromtimestamp(ts)
    @props.with_doc
    def time_received(self):
        ts = lib.dc_msg_get_received_timestamp(self._dc_msg)
        if ts:
            return datetime.utcfromtimestamp(ts)
    def get_mime_headers(self):
        import email.parser
        mime_headers = lib.dc_get_mime_headers(self._dc_context, self.id)
        if mime_headers:
            s = ffi.string(mime_headers)
            if isinstance(s, bytes):
                s = s.decode('ascii')
            return email.message_from_string(s)
    @property
    def chat(self):
        from .chatting import Chat
        chat_id = lib.dc_msg_get_chat_id(self._dc_msg)
        return Chat(self._dc_context, chat_id)
    def get_sender_contact(self):
        from .chatting import Contact
        contact_id = lib.dc_msg_get_from_id(self._dc_msg)
        return Contact(self._dc_context, contact_id)
@attr.s
class MessageType(object):
    _type = attr.ib(validator=v.instance_of(int))
    _mapping = {const.DC_MSG_TEXT: 'text', const.DC_MSG_IMAGE: 'image', const.DC_MSG_GIF: 'gif', const.DC_MSG_AUDIO: 'audio', const.DC_MSG_VIDEO: 'video', const.DC_MSG_FILE: 'file'}
    @classmethod
    def get_typecode(cls, view_type):
        for code, value in cls._mapping.items():
            if value == view_type:
                return code
        raise ValueError('message typecode not found for {!r}'.format(view_type))
    @props.with_doc
    def name(self):
        return self._mapping.get(self._type, '')
    def is_text(self):
        return self._type == const.DC_MSG_TEXT
    def is_image(self):
        return self._type == const.DC_MSG_IMAGE
    def is_gif(self):
        return self._type == const.DC_MSG_GIF
    def is_audio(self):
        return self._type == const.DC_MSG_AUDIO
    def is_video(self):
        return self._type == const.DC_MSG_VIDEO
    def is_file(self):
        return self._type == const.DC_MSG_FILE
@attr.s
class MessageState(object):
    message = attr.ib(validator=v.instance_of(Message))
    @property
    def _msgstate(self):
        return lib.dc_msg_get_state(self.message._dc_msg)
    def is_in_fresh(self):
        return self._msgstate == const.DC_STATE_IN_FRESH
    def is_in_noticed(self):
        return self._msgstate == const.DC_STATE_IN_NOTICED
    def is_in_seen(self):
        return self._msgstate == const.DC_STATE_IN_SEEN
    def is_out_preparing(self):
        return self._msgstate == const.DC_STATE_OUT_PREPARING
    def is_out_pending(self):
        return self._msgstate == const.DC_STATE_OUT_PENDING
    def is_out_failed(self):
        return self._msgstate == const.DC_STATE_OUT_FAILED
    def is_out_delivered(self):
        return self._msgstate == const.DC_STATE_OUT_DELIVERED
    def is_out_mdn_received(self):
        return self._msgstate == const.DC_STATE_OUT_MDN_RCVD