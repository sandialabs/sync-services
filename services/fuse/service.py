import os
import ast
import grp
import pwd
import sys
import time
import json
import errno
import random
import requests
import argparse
import urllib.parse

from hashlib import sha256
from fuse import FUSE, FuseOSError, ENOTSUP, Operations, fuse_get_context

config = {}

GID = grp.getgrnam("smb").gr_gid
UID = pwd.getpwnam("samba").pw_uid


def python_to_lisp(expression):
    def _recurse(x):
        if type(x) is list:
            return f"({' '.join(_recurse(y) for y in x)})"
        elif type(x) is int:
            return str(x)
        elif type(x) is float:
            return str(x)
        elif type(x) is bool:
            return "#t" if x else "#f"
        elif type(x) is str:
            if any(y in x for y in "()"):
                raise ValueError("Unable to convert string with parenthesis")
            elif any(y in x for y in "()\"'\\ \t\n\r"):
                return f'"{x}"'
            else:
                return x
        else:
            raise ValueError("Unhandled python type")

    return _recurse(expression)


def lisp_to_python(expression):
    def _recurse(x):
        if not x.startswith("("):
            if x.startswith('"') and x.endswith('"'):
                return f'"{x}"'
            elif x == "#t":
                return True
            elif x == "#f":
                return False
            elif type(ast.eval_literal(x)) is int:
                return int(x)
            elif type(ast.eval_literal(x)) is float:
                return float(x)
            else:
                return x

        tokens = []
        current = ""
        depth = 0

        for char in x[1:-1]:
            if char == "(":
                depth += 1
            elif char == ")":
                depth -= 1
            elif char == " " and depth == 0:
                if current:
                    tokens.append(current)
                    current = ""
                continue
            current += char

        if current:
            tokens.append(current)

        return [_recurse(token) for token in tokens]

    return _recurse(expression)


def query(expression):
    return lisp_to_python(
        requests.post(
            config["JOURNAL"],
            python_to_lisp(["*local*", f'"{config["SECRET"]}"', expression]),
        ).text
    )


def pathify(path):
    return [urllib.parse.unquote(x) for x in path.split("/") if x]


class Passthrough(Operations):
    def __init__(self):
        self.handles = {}

    # Helpers
    # =======

    def _full_path(self, partial):
        if partial.startswith("/"):
            partial = partial[1:]
        path = os.path.join(self.root, partial)
        return path

    # Filesystem methods
    # ==================

    def access(self, path, mode):
        print("access", path, mode, flush=True)
        return 0

    def chmod(self, path, mode):
        print("chmod", path, flush=True)
        result = query("file-system-meta-edit!", pathify(path), [["st_mode", mode]])
        if result is not True:
            raise FuseOSError(errno.EROFS)
        return 0

    def chown(self, path, uid, gid):
        print("chown", path, flush=True)
        result = query(
            "file-system-meta-edit!",
            pathify(path),
            [["st_uid", uid, "std_gid", gid]],
        )

        if result is not True:
            raise FuseOSError(errno.EROFS)
        return 0

    def getattr(self, path, fh=None):
        print("getattr", path, flush=True)
        result = query(["file-system-read-meta", ["*state*"] + pathify(path)])
        if result[0] == "object":
            return dict(result[1])
        else:
            raise FuseOSError(errno.ENOENT)

    def getxattr(self, path, name, position=0):
        print("getxattr", path, name, flush=True)
        raise FuseOSError(ENOTSUP)

    def listxattr(self, path):
        print("listxattr", path, flush=True)
        return []

    def readdir(self, path, fh):
        print("readdir", path, flush=True)
        dirents = [".", ".."]

        result = query("ledger-get", pathify(path))

        if result[0] == "directory":
            dirents.extend(result[1])
        else:
            full_path = self._full_path(path)
            if os.path.isdir(full_path):
                dirents.extend(os.listdir(full_path))

        for r in dirents:
            if r != ".dir":
                yield r

    def readlink(self, path):
        print("readlink", path, flush=True)
        # pathname = os.readlink(self._full_path(path))
        # if pathname.startswith("/"):
        #     # Path name is absolute, sanitize it.
        #     return os.path.relpath(pathname, self.root)
        # else:
        #     return pathname
        return 0

    def mknod(self, path, mode, dev):
        print("mknod", path, flush=True)
        # return os.mknod(self._full_path(path), mode, dev)
        return 0

    def rmdir(self, path):
        print("rmdir", path, flush=True)
        # full_path = self._full_path(path)
        # return os.rmdir(full_path)

        # todo: check that it is an empty file system
        query("(file-system-remove! (*state* {}))".format(pathify(path)))
        return 0

    def mkdir(self, path, mode):
        print("mkdir", path, flush=True)
        # return os.mkdir(self._full_path(path), mode)
        query(
            "(file-system-make-directory! (*state* {}) {})".format(
                pathify(path),
                "()",
            )
        )
        return 0

    def statfs(self, path):
        print("statfs", path, flush=True)
        full_path = self._full_path(path)
        stv = os.statvfs(full_path)
        return dict(
            (key, getattr(stv, key))
            for key in (
                "f_bavail",
                "f_bfree",
                "f_blocks",
                "f_bsize",
                "f_favail",
                "f_ffree",
                "f_files",
                "f_flag",
                "f_frsize",
                "f_namemax",
            )
        )

    def unlink(self, path):
        print("unlink", path, flush=True)
        # todo: make sure it's a file
        query("(file-system-remove! (*state* {}))".format(pathify(path)))
        return

    def symlink(self, name, target):
        print("symlink", target, flush=True)
        # return os.symlink(target, self._full_path(name))
        query(
            "(file-system-link! (*state* {}) (*state* {}))".format(
                pathify(name),
                pathify(target),
            )
        )
        return 0

    def rename(self, old, new):
        print("rename", old, flush=True)
        query(
            "(file-system-rename! (*state* {}) (*state* {}))".format(
                pathify(old),
                pathify(new),
            )
        )
        return 0

    def link(self, target, name):
        print("link", target, flush=True)
        # return os.link(self._full_path(name), self._full_path(target))
        # todo: return some kind of error
        return 0

    def utimens(self, path, times=None):
        print("utimens", path, flush=True)
        # return os.utime(self._full_path(path), times)
        now = time.time()
        query(
            "(file-system-edit-meta! (*state* {}) {})".format(
                pathify(path),
                json2lisp({"st_atime": now, "st_mtime": now}),
            )
        )
        return 0

    # File methods
    # ============

    def open(self, path, flags):
        print("opening", path, flags, flush=True)
        # full_path = self._full_path(path)
        # return os.open(full_path, flags)

        # how would i use flags to detect if this is a write situation?
        handle = random.randint(1, 1000000)

        if flags & os.O_WRONLY or flags & os.O_RDWR:
            self.handles[handle] = path

        return handle

    def create(self, path, mode, fi=None):
        print("creating", path, flush=True)
        # uid, gid, pid = fuse_get_context()
        # full_path = self._full_path(path)
        # fd = os.open(full_path, os.O_WRONLY | os.O_CREAT, mode)
        # os.chown(full_path, uid, gid)  #chown to context uid & gid
        # return fd
        info = {
            "attributes": {
                "st_atime": time.time(),
                "st_ctime": time.time(),
                "st_gid": grp.getgrnam("smb").gr_gid,
                "st_mode": 33188,
                "st_mtime": time.time(),
                "st_nlink": 1,
                "st_size": 1,
                "st_uid": pwd.getpwnam("samba").pw_uid,
            },
        }

        print(
            "query:",
            query(
                '(ledger-set! (*state* ) "{}")'.format(
                    pathify(path),
                    urllib.parse.quote(json.dumps(info)),
                )
            ),
            flush=True,
        )
        fh = random.randint(0, 2**20)
        self.handles[fh] = []
        return fh

    def read(self, path, length, offset, fh):
        print("reading", path, length, offset, flush=True)
        result = parse(query("(ledger-get (*state* {}))".format(pathify(path))))
        print(result, flush=True)

        if result[0] == "object":
            payload = json.loads(urllib.parse.unquote(result[1]))
            if digest := payload.get("digest"):
                print("digest", digest)
                with open(os.path.join(self.root, digest), "rb") as fd:
                    fd.seek(offset)
                    return fd.read(length)
            else:
                return b""
        else:
            raise ValueError("read error")
        # else:
        #     os.lseek(fh, offset, os.SEEK_SET)
        #     return os.read(fh, length)

    def write(self, path, buf, offset, fh):
        print("writing", path, len(buf), offset, fh, flush=True)

        self.handles[fh].append(buf)
        # digest = sha256(buf).hexdigest()

        # print(os.path.join(self.root, digest), flush=True)
        # with open(os.path.join(self.root, digest), "wb") as fd:
        #     fd.write(buf)

        # os.lseek(fh, offset, os.SEEK_SET)
        # return os.write(fh, buf)
        return len(buf)

    def truncate(self, path, length, fh=None):
        print("truncating", path, flush=True)
        # full_path = self._full_path(path)
        # with open(full_path, "r+") as f:
        #     f.truncate(length)
        return 0

    def flush(self, path, fh):
        print("flushing", path, flush=True)
        # return os.fsync(fh)
        return 0

    def release(self, path, fh):
        print("releasing", path, fh, flush=True)

        if self.handles.get(fh):
            document = b"".join(self.handles[fh])

            digest = sha256(document).hexdigest()
            print("released:", fh, document, digest, flush=True)

            print(os.path.join(self.root, digest), flush=True)
            with open(os.path.join(self.root, digest), "wb") as fd:
                fd.write(document)

            print(self.handles)

            info = {
                "digest": digest,
                "attributes": {
                    "st_atime": time.time(),
                    "st_ctime": time.time(),
                    "st_gid": grp.getgrnam("smb").gr_gid,
                    "st_mode": 33188,
                    "st_mtime": time.time(),
                    "st_nlink": 1,
                    "st_size": len(document),
                    "st_uid": pwd.getpwnam("samba").pw_uid,
                },
            }

            query(
                '(ledger-set! (*state* {}) "{}")'.format(
                    pathify(path),
                    urllib.parse.quote(json.dumps(info)),
                )
            )

        return 0

    def fsync(self, path, fdatasync, fh):
        print("fsyncing", path, flush=True)
        # return self.flush(path, fh)
        return 0


def main(mountpoint, root):
    FUSE(
        Passthrough(),
        mountpoint,
        nothreads=True,
        foreground=True,
        allow_other=True,
        fsname="",
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-j",
        "--journal",
        type=str,
        help="Full-qualified path to the journal SDK instance",
    )
    parser.add_argument(
        "-s",
        "--secret",
        type=str,
        help="The secret variable to access the journal",
    )
    parser.add_argument(
        "-b",
        "--back",
        type=str,
        help="Backend directory to store files",
    )
    parser.add_argument(
        "-f",
        "--front",
        type=str,
        default=None,
        help="Frontend directory to expose to user",
    )

    args = parser.parse_args()

    config["JOURNAL"] = args.journal if args.journal else os.getenv("JOURNAL")
    config["SECRET"] = args.secret if args.journal else os.getenv("SECRET")

    main(args.front, args.back)
