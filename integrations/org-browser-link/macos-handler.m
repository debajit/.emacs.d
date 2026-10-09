// A local, unsandboxed URL handler forwards requests to the existing Emacs
// server.  Pass the URL as an argument, never as shell commands or Lisp code.
// https://developer.apple.com/documentation/appkit/nsapplicationdelegate/application(_:open:)
// https://developer.apple.com/documentation/bundleresources/information-property-list/cfbundleurltypes
#import <Cocoa/Cocoa.h>

static int forwardURL(NSURL *url) {
    if (![url.scheme.lowercaseString isEqualToString:@"org-protocol"]) return 1;
    NSString *client = [[NSBundle mainBundle] objectForInfoDictionaryKey:@"EmacsClientPath"];
    if (!client) return 1;
    NSTask *task = [[NSTask alloc] init];
    task.executableURL = [NSURL fileURLWithPath:client];
    task.arguments = @[@"--quiet", @"--no-wait", @"--", url.absoluteString];
    NSError *error = nil;
    if (![task launchAndReturnError:&error]) {
        NSLog(@"Emacs Org Protocol: %@", error.localizedDescription);
        return 1;
    }
    [task waitUntilExit];
    if (task.terminationStatus != 0) {
        NSLog(@"Emacs Org Protocol: emacsclient exited with %d", task.terminationStatus);
    }
    return task.terminationStatus;
}

@interface OrgProtocolDelegate : NSObject <NSApplicationDelegate>
@end

@implementation OrgProtocolDelegate
- (void)application:(NSApplication *)application openURLs:(NSArray<NSURL *> *)urls {
    for (NSURL *url in urls) {
        forwardURL(url);
    }
    [application terminate:nil];
}
@end

int main(int argc, const char *argv[]) {
    @autoreleasepool {
        // Exercise the same forwarding path without a GUI in integration tests.
        if (argc == 3 && strcmp(argv[1], "--url") == 0) {
            return forwardURL([NSURL URLWithString:[NSString stringWithUTF8String:argv[2]]]);
        }
        NSApplication *application = [NSApplication sharedApplication];
        [application setActivationPolicy:NSApplicationActivationPolicyProhibited];
        __attribute__((objc_precise_lifetime)) OrgProtocolDelegate *delegate = [[OrgProtocolDelegate alloc] init];
        application.delegate = delegate;
        [application run];
    }
    return 0;
}
