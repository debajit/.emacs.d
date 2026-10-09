#!/usr/bin/env python3
"""Register org-protocol with the existing emacsclient on macOS or Linux.

Reference: https://orgmode.org/worg/org-contrib/org-protocol.html
macOS uses a local app rather than changing the existing Emacs server socket.
"""

import argparse
import ctypes
import os
from pathlib import Path
import plistlib
import shutil
import subprocess
import sys
import tempfile

BUNDLE_ID = "local.emacs.org-protocol"
SOURCE = Path(__file__).resolve().parent


def register_macos(bundle):
    """Register the app and select it for org-protocol URLs only."""
    subprocess.run([
        "/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister",
        "-f", str(bundle),
    ], check=True)
    # https://developer.apple.com/documentation/coreservices/1447760-lssetdefaulthandlerforurlscheme
    core = ctypes.CDLL("/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation")
    services = ctypes.CDLL("/System/Library/Frameworks/CoreServices.framework/CoreServices")
    core.CFStringCreateWithCString.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_uint32]
    core.CFStringCreateWithCString.restype = ctypes.c_void_p
    core.CFRelease.argtypes = [ctypes.c_void_p]
    services.LSSetDefaultHandlerForURLScheme.argtypes = [ctypes.c_void_p, ctypes.c_void_p]
    services.LSSetDefaultHandlerForURLScheme.restype = ctypes.c_int32
    scheme = core.CFStringCreateWithCString(None, b"org-protocol", 0x08000100)
    identifier = core.CFStringCreateWithCString(None, BUNDLE_ID.encode(), 0x08000100)
    try:
        status = services.LSSetDefaultHandlerForURLScheme(scheme, identifier)
        if status:
            raise RuntimeError(f"Launch Services registration failed: {status}")
    finally:
        core.CFRelease(scheme)
        core.CFRelease(identifier)


def install_macos(client, destination, register):
    bundle = destination / "Emacs Org Protocol.app"
    if bundle.exists():
        info = plistlib.loads((bundle / "Contents/Info.plist").read_bytes())
        if info.get("CFBundleIdentifier") != BUNDLE_ID:
            raise RuntimeError(f"Refusing to replace another application: {bundle}")
    with tempfile.TemporaryDirectory(prefix="emacs-org-protocol-") as temporary:
        staging = Path(temporary) / bundle.name
        contents = staging / "Contents"
        executable = contents / "MacOS/EmacsOrgProtocol"
        executable.parent.mkdir(parents=True)
        subprocess.run([
            "/usr/bin/clang", "-fobjc-arc", "-Wall", "-Wextra", "-Werror",
            "-framework", "Cocoa", str(SOURCE / "macos-handler.m"),
            "-o", str(executable),
        ], check=True)
        info = {
            "CFBundleIdentifier": BUNDLE_ID,
            "CFBundleName": "Emacs Org Protocol",
            "CFBundleExecutable": executable.name,
            "CFBundlePackageType": "APPL",
            "CFBundleVersion": "1",
            "LSUIElement": True,
            "EmacsClientPath": str(client),
            "CFBundleURLTypes": [{
                "CFBundleURLName": "Org Protocol",
                "CFBundleTypeRole": "Editor",
                "CFBundleURLSchemes": ["org-protocol"],
            }],
        }
        (contents / "Info.plist").write_bytes(plistlib.dumps(info))
        subprocess.run(["/usr/bin/codesign", "--force", "--sign", "-", str(staging)], check=True)
        destination.mkdir(parents=True, exist_ok=True)
        shutil.copytree(staging, bundle, dirs_exist_ok=True)
    if register:
        register_macos(bundle)
    return bundle


def desktop_entry(client):
    """Quote an executable according to the Desktop Entry Exec specification.

    https://specifications.freedesktop.org/desktop-entry-spec/latest/exec-variables.html
    """
    executable = str(client).replace("%", "%%")
    for character in ('\\', '"', '`', '$'):
        executable = executable.replace(character, "\\" + character)
    executable = executable.replace("\\", "\\\\")
    return (
        "[Desktop Entry]\nType=Application\nName=Emacs Org Protocol\n"
        f'Exec="{executable}" --quiet --no-wait -- %u\n'
        "Terminal=false\nNoDisplay=true\nIcon=emacs\n"
        "MimeType=x-scheme-handler/org-protocol;\n"
    )


def install_linux(client, destination, register):
    entry = destination / "emacs-org-protocol.desktop"
    destination.mkdir(parents=True, exist_ok=True)
    entry.write_text(desktop_entry(client))
    if register:
        if shutil.which("update-desktop-database"):
            subprocess.run(["update-desktop-database", str(destination)], check=True)
        subprocess.run([
            "xdg-mime", "default", entry.name, "x-scheme-handler/org-protocol",
        ], check=True)
    return entry


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--emacsclient", default=shutil.which("emacsclient"))
    parser.add_argument("--destination", type=Path)
    parser.add_argument("--no-register", action="store_true", help="Build without changing URL associations")
    args = parser.parse_args()
    if not args.emacsclient or not Path(args.emacsclient).is_file():
        parser.error("emacsclient was not found; provide --emacsclient /absolute/path/to/emacsclient")
    client = Path(args.emacsclient).resolve()
    if sys.platform == "darwin":
        destination = args.destination or Path.home() / "Applications"
        installed = install_macos(client, destination.expanduser().resolve(), not args.no_register)
    elif sys.platform.startswith("linux"):
        destination = args.destination or Path(os.environ.get("XDG_DATA_HOME", Path.home() / ".local/share")) / "applications"
        installed = install_linux(client, destination.expanduser().resolve(), not args.no_register)
    else:
        parser.error("This installer supports macOS and Linux")
    print(f"Installed: {installed}")
    if not args.no_register:
        print("Registered org-protocol with your existing Emacs server.")


if __name__ == "__main__":
    main()
