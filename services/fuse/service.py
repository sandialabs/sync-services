# initial code taken from: https://www.stavros.io/posts/python-fuse-filesystem/

import os
import grp
import pwd
import sys
import errno
import requests
import argparse
import urllib.parse
from hashlib import sha256

from fuse import FUSE, FuseOSError, Operations, fuse_get_context

config = {}


def query(expression, is_local=True):
    if is_local:
        return requests.post(
            config['JOURNAL'], '(*local* "{secret}" {expression})'.format(
                secret=config['SECRET'],
                expression=expression,
            )).text
    else:
        return requests.post(
            config['JOURNAL'],
            expression,
        ).text


def pathify(path):
    return ' '.join(urllib.parse.unquote(x) for x in path.split('/') if x)


def parse(expression):
    print(expression, flush=True)
    if expression.startswith('(directory '):
        return ('directory',
                [urllib.parse.quote(x) for x in expression[12:-5].split(' ')])
    elif expression.startswith('(object '):
        return ('object', bytes.fromhex(expression[9:-2]))
    elif expression.startswith('(nothing '):
        return ('nothing', None)
    elif expression.startswith('(unknown '):
        return ('unknown', None)
    else:
        print('Error: ', expression, flush=True)
        return ('error', None)


class Passthrough(Operations):

    def __init__(self, root):
        self.root = root

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
        print('access', path, flush=True)
        # full_path = self._full_path(path)
        # if not os.access(full_path, mode):
        #     raise FuseOSError(errno.EACCES)
        return

    def chmod(self, path, mode):
        print('chmod', path, flush=True)
        full_path = self._full_path(path)
        return os.chmod(full_path, mode)

    def chown(self, path, uid, gid):
        print('chown', path, flush=True)
        full_path = self._full_path(path)
        return os.chown(full_path, uid, gid)

    def getattr(self, path, fh=None):
        print('getattr', path, flush=True)
        gid = grp.getgrnam('smb').gr_gid
        uid = pwd.getpwnam('samba').pw_uid
        dir_default = {
            'st_atime': 1749684549.1848755,
            'st_ctime': 1749678764.6812148,
            'st_gid': gid,
            'st_mode': 16877,
            'st_mtime': 1749678764.6812148,
            'st_nlink': 2,
            'st_size': 4096,
            'st_uid': uid,
        }
        obj_default = {
            'st_atime': 1749678709.2532182,
            'st_ctime': 1749678733.6892166,
            'st_gid': gid,
            'st_mode': 33188,
            'st_mtime': 1749678733.6892166,
            'st_nlink': 1,
            'st_size': 4096,
            'st_uid': uid,
        }

        result = parse(query('(ledger-get (*state* {}))'.format(
            pathify(path))))
        if result[0] == 'directory':
            return dir_default
        elif result[0] == 'object':
            return obj_default
        else:
            full_path = self._full_path(path)
            st = os.lstat(self._full_path(path))
            return dict((key, getattr(st, key))
                    for key in ('st_atime', 'st_ctime', 'st_gid', 'st_mode',
                                'st_mtime', 'st_nlink', 'st_size', 'st_uid'))

    def readdir(self, path, fh):
        print('readdir', path, flush=True)
        dirents = ['.', '..']

        result = parse(query('(ledger-get (*state* {}))'.format(
            pathify(path))))

        if result[0] == 'directory':
            dirents.extend(result[1])
        else:
            full_path = self._full_path(path)
            if os.path.isdir(full_path):
                dirents.extend(os.listdir(full_path))

        for r in dirents:
            yield r

    def readlink(self, path):
        print('readlink', path, flush=True)
        pathname = os.readlink(self._full_path(path))
        if pathname.startswith("/"):
            # Path name is absolute, sanitize it.
            return os.path.relpath(pathname, self.root)
        else:
            return pathname

    def mknod(self, path, mode, dev):
        print('mknod', path, flush=True)
        return os.mknod(self._full_path(path), mode, dev)

    def rmdir(self, path):
        print('rmdir', path, flush=True)
        full_path = self._full_path(path)
        return os.rmdir(full_path)

    def mkdir(self, path, mode):
        print('mkdir', path, flush=True)
        return os.mkdir(self._full_path(path), mode)

    def statfs(self, path):
        print('statfs', path, flush=True)
        full_path = self._full_path(path)
        stv = os.statvfs(full_path)
        return dict((key, getattr(stv, key))
                    for key in ('f_bavail', 'f_bfree', 'f_blocks', 'f_bsize',
                                'f_favail', 'f_ffree', 'f_files', 'f_flag',
                                'f_frsize', 'f_namemax'))

    def unlink(self, path):
        print('unlink', path, flush=True)
        # delete file here
        return os.unlink(self._full_path(path))

    def symlink(self, name, target):
        print('symlink', target, flush=True)
        return os.symlink(target, self._full_path(name))

    def rename(self, old, new):
        print('renmae', old, flush=True)
        return os.rename(self._full_path(old), self._full_path(new))

    def link(self, target, name):
        print('link', target, flush=True)
        return os.link(self._full_path(name), self._full_path(target))

    def utimens(self, path, times=None):
        print('utimens', path, flush=True)
        return os.utime(self._full_path(path), times)

    # File methods
    # ============

    def open(self, path, flags):
        print('opening', path, flush=True)
        # full_path = self._full_path(path)
        # return os.open(full_path, flags)
        return 0

    def create(self, path, mode, fi=None):
        print('creating', path, flush=True)
        # uid, gid, pid = fuse_get_context()
        # full_path = self._full_path(path)
        # fd = os.open(full_path, os.O_WRONLY | os.O_CREAT, mode)
        # os.chown(full_path, uid, gid)  #chown to context uid & gid
        # return fd
        query('(ledger-set! (*state* {}) "{}")'.format(
            pathify(path),
            '()',
        ))
        return 0

    def read(self, path, length, offset, fh):
        print('reading', path, flush=True)
        result = parse(query('(ledger-get (*state* {}))'.format(
            pathify(path))))

        if result[0] == 'object':
            return result[1]
        else:
            os.lseek(fh, offset, os.SEEK_SET)
            return os.read(fh, length)

    def write(self, path, buf, offset, fh):
        print('writing', path, flush=True)
        # make sure it's only in data/stage
        # store equivaelnt in
        query('(ledger-set! (*state* {}) "{}")'.format(
            pathify(path),
            sha256(buf).hexdigest(),
        ))
        # os.lseek(fh, offset, os.SEEK_SET)
        # return os.write(fh, buf)
        return 0

    def truncate(self, path, length, fh=None):
        print('truncating', path, flush=True)
        full_path = self._full_path(path)
        with open(full_path, 'r+') as f:
            f.truncate(length)

    def flush(self, path, fh):
        print('flushing', path, flush=True)
        return os.fsync(fh)

    def release(self, path, fh):
        print('releasing', path, flush=True)
        # return os.close(fh)
        return 0

    def fsync(self, path, fdatasync, fh):
        print('fsyncing', path, flush=True)
        return self.flush(path, fh)


def main(mountpoint, root):
    FUSE(
        Passthrough(root),
        mountpoint,
        nothreads=True,
        foreground=True,
        allow_other=True,
    )


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '-j',
        '--journal',
        type=str,
        help='Full-qualified path to the journal SDK instance',
    )
    parser.add_argument(
        '-s',
        '--secret',
        type=str,
        help='The secret variable to access the journal',
    )
    parser.add_argument(
        '-b',
        '--back',
        type=str,
        help='Backend directory to store files',
    )
    parser.add_argument(
        '-f',
        '--front',
        type=str,
        default=None,
        help='Frontend directory to expose to user',
    )

    args = parser.parse_args()

    config['JOURNAL'] = args.journal if args.journal else os.getenv('JOURNAL')
    config['SECRET'] = args.secret if args.journal else os.getenv('SECRET')

    main(args.front, args.back)
