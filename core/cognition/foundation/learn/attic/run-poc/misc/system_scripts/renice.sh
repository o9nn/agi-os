#! /bin/bash
ps ax |grep postgres |grep pairs | cut -b1-5 | sudo xargs renice 10