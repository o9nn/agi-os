import tkinter as tk
class Tooltip:
    def __init__(self, widget, text='widget info', delay=500, wraplength=180, background='#FFFFEA', foreground='black'):
        self.widget = widget
        self.text = text
        self.delay = delay
        self.wraplength = wraplength
        self.background = background
        self.foreground = foreground
        self.widget_bind_enter = widget.bind('<Enter>', self.on_enter)
        self.widget_bind_leave = widget.bind('<Leave>', self.on_leave)
        self.widget_bind_button = widget.bind('<ButtonPress>', self.on_leave)
        self.tooltip_window = None
        self.schedule_id = None
    def on_enter(self, event=None):
        self.schedule_id = self.widget.after(self.delay, self.show_tooltip)
    def on_leave(self, event=None):
        if self.schedule_id:
            self.widget.after_cancel(self.schedule_id)
            self.schedule_id = None
        self.hide_tooltip()
    def show_tooltip(self):
        x, y, _, _ = self.widget.bbox('insert')
        x += self.widget.winfo_rootx() + 25
        y += self.widget.winfo_rooty() + 25
        self.tooltip_window = tk.Toplevel(self.widget)
        self.tooltip_window.wm_overrideredirect(True)
        self.tooltip_window.wm_geometry(f'+{x}+{y}')
        label = tk.Label(self.tooltip_window, text=self.text, justify='left', background=self.background, foreground=self.foreground, relief='solid', borderwidth=1, wraplength=self.wraplength)
        label.pack(ipadx=1)
    def hide_tooltip(self):
        if self.tooltip_window:
            self.tooltip_window.destroy()
            self.tooltip_window = None