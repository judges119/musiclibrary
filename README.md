# Music Library

When this is started by running `musicdaemon(["path_to_library1", "path_to_libraryN"], "path_to_sqlite_db")`in an Erlang shell, the program periodically/manually recursively scans all directories and collect information about any MP3s found from their ID3v2 tags, as well as taking an MD5 hash of them. Files without an entry in the database are added, files with a different hash from the one stored will be used to update the DB and duplicate entries will be ignored. Another periodic/manual process compares database entries to the paths to determine if they still exist; removing entries about missing files.

The main.cpp file should be compiled with the flags specified in it and then put into your path, this handles the actual MD5 hashing instead of Erlang (future iterations will hopefully make use of OpenCL).

### MIT License
Copyright (c) 2014 Adam O'Grady

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.