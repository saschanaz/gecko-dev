// |jit-test| error:Error
// FIXME: Reenable this test, see bug XXX.

if (typeof parseRegExp === 'undefined')
    quit();

load(libdir + "regexp_parse.js");

test_mix("\\0", all_flags,
         Atom("\u0000"));
