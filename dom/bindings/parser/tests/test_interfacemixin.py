import WebIDL

def WebIDLTest(parser, harness):
    parser.parse("interface mixin Foo { };")
    results = parser.finish()
    harness.ok(True, "Empty interface mixin parsed without error.")
    harness.check(len(results), 1, "Should be one production")
    harness.ok(isinstance(results[0], WebIDL.IDLInterfaceMixin),
               "Should be an IDLInterface")
    mixin = results[0]
    harness.check(mixin.identifier.QName(), "::Foo", "Interface has the right QName")
    harness.check(mixin.identifier.name, "Foo", "Interface has the right name")

    parser = parser.reset()
    parser.parse("""
        interface mixin QNameBase {
            attribute long foo;
        };
    """)
    results = parser.finish()
    harness.check(len(results), 1, "Should be one productions")
    harness.ok(isinstance(results[0], WebIDL.IDLInterfaceMixin),
               "Should be an IDLInterface")
    harness.check(len(results[0].members), 1, "Expect 1 productions")
    mixin = results[0]
    harness.check(mixin.members[0].identifier.QName(), "::QNameBase::foo",
                  "Member has the right QName")

    parser = parser.reset()
    parser.parse("""
        interface mixin A {
            readonly attribute boolean x;
            void foo();
        };
        partial interface mixin A {
            readonly attribute boolean y;
            void foo(long arg);
        };
    """)
    results = parser.finish()
    harness.check(len(results), 2,
                  "Should have two results with partial interface mixin")
    mixin = results[0]
    harness.check(len(mixin.members), 3,
                  "Should have three members with partial interface mixin")
    harness.check(mixin.members[0].identifier.name, "x",
                  "First member should be x with partial interface mixin")
    harness.check(mixin.members[1].identifier.name, "foo",
                  "Second member should be foo with partial interface mixin")
    harness.check(len(mixin.members[1].signatures()), 2,
                  "Should have two foo signatures with partial interface mixin")
    harness.check(mixin.members[2].identifier.name, "y",
                  "Third member should be y with partial interface mixin")

    parser = parser.reset()
    parser.parse("""
        partial interface mixin A {
            readonly attribute boolean y;
            void foo(long arg);
        };
        interface mixin A {
            readonly attribute boolean x;
            void foo();
        };
    """)
    results = parser.finish()
    harness.check(len(results), 2,
                  "Should have two results with reversed partial interface mixin")
    mixin = results[1]
    harness.check(len(mixin.members), 3,
                  "Should have three members with reversed partial interface mixin")
    harness.check(mixin.members[0].identifier.name, "x",
                  "First member should be x with reversed partial interface mixin")
    harness.check(mixin.members[1].identifier.name, "foo",
                  "Second member should be foo with reversed partial interface mixin")
    harness.check(len(mixin.members[1].signatures()), 2,
                  "Should have two foo signatures with reversed partial interface mixin")
    harness.check(mixin.members[2].identifier.name, "y",
                  "Third member should be y with reversed partial interface mixin")

    parser = parser.reset()
    parser.parse("""
        interface Interface {};
        interface mixin Mixin {
            attribute short x;
        };
        Interface includes Mixin;
    """)
    results = parser.finish()
    iface = results[0]
    harness.check(len(iface.members), 1, "Should merge members from mixins")
    harness.check(iface.members[0].identifier.name, "x",
                  "Should merge members from mixins")

    parser = parser.reset()
    threw = False
    try:
        parser.parse("""
            interface mixin A {
                readonly attribute boolean x;
            };
            interface mixin A {
                readonly attribute boolean y;
            };
        """)
        results = parser.finish()
    except:
        threw = True
    harness.ok(threw,
               "Should not allow two non-partial interface mixins with the same name")

    parser = parser.reset()
    threw = False
    try:
        parser.parse("""
            partial interface mixin A {
                readonly attribute boolean x;
            };
            partial interface mixin A {
                readonly attribute boolean y;
            };
        """)
        results = parser.finish()
    except:
        threw = True
    harness.ok(threw,
               "Must have a non-partial interface mixin for a given name")

    parser = parser.reset()
    threw = False
    try:
        parser.parse("""
            dictionary A {
                boolean x;
            };
            partial interface mixin A {
                readonly attribute boolean y;
            };
        """)
        results = parser.finish()
    except:
        threw = True
    harness.ok(threw,
               "Should not allow a name collision between partial interface "
               "mixin and other object")

    parser = parser.reset()
    threw = False
    try:
        parser.parse("""
            dictionary A {
                boolean x;
            };
            interface mixin A {
                readonly attribute boolean y;
            };
        """)
        results = parser.finish()
    except:
        threw = True
    harness.ok(threw,
               "Should not allow a name collision between interface mixin "
               "and other object")

    parser = parser.reset()
    threw = False
    try:
        parser.parse("""
            interface mixin A {
                readonly attribute boolean x;
            };
            interface A;
        """)
        results = parser.finish()
    except:
        threw = True
    harness.ok(threw,
               "Should not allow a name collision between external interface "
               "and interface mixin")

    parser = parser.reset()
    threw = False
    try:
        parser.parse("""
            [SomeRandomAnnotation]
            interface mixin A {
                readonly attribute boolean y;
            };
        """)
        results = parser.finish()
    except:
        threw = True
    harness.ok(threw,
               "Should not allow unknown extended attributes on interface mixins")

    parser = parser.reset()
    threw = False
    try:
        parser.parse("""
            interface mixin A {
                getter double (DOMString propertyName);
            };
        """)
        results = parser.finish()
    except:
        threw = True
    harness.ok(threw,
               "Should not allow getters on interface mixins")

    parser = parser.reset()
    threw = False
    try:
        parser.parse("""
            interface mixin A {
                setter void (DOMString propertyName, double propertyValue);
            };
        """)
        results = parser.finish()
    except:
        threw = True
    harness.ok(threw,
               "Should not allow setters on interface mixins")

    parser = parser.reset()
    threw = False
    try:
        parser.parse("""
            interface mixin A {
                deleter void (DOMString propertyName);
            };
        """)
        results = parser.finish()
    except:
        threw = True
    harness.ok(threw,
               "Should not allow deleters on interface mixins")

    parser = parser.reset()
    threw = False
    try:
        parser.parse("""
            interface mixin A {
                legacycaller double compute(double x);
            };
        """)
        results = parser.finish()
    except:
        threw = True
    harness.ok(threw,
               "Should not allow legacycallers on interface mixins")

    parser = parser.reset()
    threw = False
    try:
        parser.parse("""
            interface mixin A {
                inherit attribute x;
            };
        """)
        results = parser.finish()
    except:
        threw = True
    harness.ok(threw,
               "Should not allow inherited attribute on interface mixins")
