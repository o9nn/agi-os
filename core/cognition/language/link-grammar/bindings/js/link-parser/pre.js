Module.preRun = Module.preRun || []
Module.preRun.push(function () {
  FS.mkdir('/data')
  FS.mount(NODEFS, {root: __dirname + '/data'}, '/data');
  function readline () {
    var BUFSIZE = 256;
    var buf = Buffer.alloc ? Buffer.alloc(BUFSIZE) : new Buffer(BUFSIZE);
    var bytesRead = 0;
    var fd = process.stdin.fd;
    var usingDevice = false;
    if (process.platform !== 'win32') {
      try {
        fd = fs.openSync('/dev/stdin', 'r');
        usingDevice = true;
      } catch (e) {}
    }
    bytesRead = fs.readSync(fd, buf, 0, BUFSIZE, null);
    if (usingDevice) {
      fs.closeSync(fd);
    }
    return buf.slice(0, bytesRead).toString('utf-8');
  }
  TTY.default_tty_ops.get_char = function (tty) {
    if (!tty.input.length) {
      tty.input = intArrayFromString(readline(), true);
      tty.input.push(null); 
    }
    return tty.input.shift();
  };
  TTY.default_tty_ops.put_char = function (tty, val) {
    process.stdout.write(UTF8ArrayToString([val], 0));
  };
  TTY.default_tty_ops.flush = function (tty) {};
  TTY.default_tty1_ops.put_char = function (tty, val) {
    process.stderr.write(UTF8ArrayToString([val], 0));
  };
  TTY.default_tty1_ops.flush = function (tty) {};
})