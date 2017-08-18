import os
import sys
import platform
import shutil
import subprocess
import pymake

fc = 'gfortran'
cc = 'gcc'

def create_dir(pth):
    # remove pth directory if it exists
    if os.path.exists(pth):
        print('removing... {}'.format(os.path.abspath(pth)))
        shutil.rmtree(pth)
    # create pth directory
    print('creating... {}'.format(os.path.abspath(pth)))
    os.makedirs(pth)

    msg = 'could not create... {}'.format(os.path.abspath(pth))
    assert os.path.exists(pth), msg
    
    return

def test_create_dirs():
    pths = [os.path.join('..', 'bin'),
            os.path.join('temp')]

    for pth in pths:
        create_dir(pth)

    return

def set_compiler():
    fct = fc
    cct = cc
    # parse command line arguments to see if user specified options
    # relative to building the target
    msg = ''
    for idx, arg in enumerate(sys.argv):
        if arg.lower() == '--ifort':
            if len(msg) > 0:
                msg += '\n'
            msg += '{} - '.format(arg.lower()) + \
                   '{} will be built with ifort.'.format(starget)
            fct = 'ifort'
        elif arg.lower() == '--cl':
            if len(msg) > 0:
                msg += '\n'
            msg += '{} - '.format(arg.lower()) + \
                   '{} will be built with cl.'.format(starget)
            cct = 'cl'
        elif arg.lower() == '--clang':
            if len(msg) > 0:
                msg += '\n'
            msg += '{} - '.format(arg.lower()) + \
                   '{} will be built with clang.'.format(starget)
            cct = 'clang'
    if len(msg) > 0:
        print(msg)
        
    return fct, cct


def test_build_errortest():
    # set source and target paths
    srcdir = os.path.join('..', 'src')
    target = os.path.join('..', 'bin', 'gfortran7-uzf')
    srcdir2 = None

    build(srcdir, srcdir2, target, 'gfortran7-uzf')

    msg = '{} does not exist.'.format(os.path.relpath(target))
    assert os.path.isfile(target), msg

    
def test_errortest():
    # set path
    pth = './'
    
    # run gfortran7-uzf
    argv = [os.path.join('..', 'bin', 'gfortran7-uzf')]
    buff, ierr = run_command(argv, pth)
    msg = '\nERROR: {} did not terminate normally\n\n'.format(argv[0])
    msg += '{}'.format(buff)
    assert 'Normal termination' in buff, msg


def build(srcdir, srcdir2, target, starget, extrafiles=None):
    """
    Build a specified target
    """
    debug = False
    fflags = None

    fct, cct = set_compiler()

    # parse remaining command line arguments to see if user specified options
    # relative to building the target
    msg = ''
    for idx, arg in enumerate(sys.argv):
        if arg.lower() == '--debug':
            debug = True
            msg += '{} - '.format(arg.lower()) + \
                   '{} will be built with debug flags.'.format(starget)
        elif arg.lower() == '--fflags':
            if len(sys.argv) > idx + 1:
                t = sys.argv[idx + 1:]
                fflags = ''
                for tt in t:
                    fflags += tt + ' '
                break
    if len(msg) > 0:
        print(msg)

    # write message to log
    txt = 'checking if {} should be built'.format(starget)
    print(txt)
    # determine if application should be built
    for idx, arg in enumerate(sys.argv):
        if arg.lower() == '--nobuild':
            print('{} will not be built'.format(starget))
            return

    # make sure exe extension is used on windows
    sysinfo = platform.system()
    if sysinfo.lower() == 'windows':
        filename, fileext = os.path.splitext(target)
        if fileext.lower() != '.exe':
            target += '.exe'

    # call main -- note that this form allows main to be called
    # from python as a function.
    success = pymake.pymake.main(srcdir, target, fct, cct,
                                 include_subdirs=True,
                                 srcdir2=srcdir2,
                                 debug=debug, extrafiles=extrafiles,
                                 fflags=fflags)

    msg = 'Could not build {}'.format(target)
    assert success == 0, msg
    
    return


def run_command(argv, pth, timeout=10):
    buff = ''
    ierr = 0
    with subprocess.Popen(argv,
                          stdout=subprocess.PIPE,
                          stderr=subprocess.STDOUT,
                          cwd=pth) as process:
        try:
            output, unused_err = process.communicate(timeout=timeout)
            buff = output.decode('utf-8')
        except subprocess.TimeoutExpired:
            process.kill()
            output, unused_err = process.communicate()
            buff = output.decode('utf-8')
            ierr = 100
        except:
            output, unused_err = process.communicate()
            buff = output.decode('utf-8')
            ierr = 101

    return buff, ierr


if __name__ == "__main__":
    test_create_dirs()
    test_build_errortest()
    test_errortest()
    
