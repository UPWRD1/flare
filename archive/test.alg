[src/root/passes/midend/environment.rs:115:9] self = Environment {
    file_table: Table {
        entries: {
            (
                2,
                "/workspaces/allegro/allegro/examples/result.alg",
            ): FileTableEntry {
                path: "/workspaces/allegro/allegro/examples/result.alg",
                src: "type Result<?T, ?E> = enum of \n    Ok(?T),\n    Err(?E)",
            },
            (
                1,
                "examples/slice.alg",
            ): FileTableEntry {
                path: "examples/slice.alg",
                src: "with \"examples/result\"\n\ntype Slice<?T> = struct of\n\tstart: ^?T,\n\tlen: int,\n\tcap: int\n\ndef Slice<?T> =\n    let len of self =\n        self.len\n\n    let new of x = \n        Slice {start: %x, len: 1, cap: 32}\n\n    let get of self, idx: int -> ?T =\n        if idx <= self.len then\n            return idx * self.start\n        else \n            return -1\nend\n\nlet containshelper of s: Slice<?T>, e: ?T, c: int -> bool =\n    if c < s.len() then\n       if s.get(c).unwrap() == e then \n            return true \n       else \n            return containshelper(s, e, c + 1)\n    else \n        return false\n\ndef Slice<?T> =\n    let contains of self, item: ?T -> bool =\n        containshelper(self, item, 0)\nend\n\n\n",
            },
        },
    },
    module_table: Table {
        entries: {},
    },
    function_table: Table {
        entries: {
            (
                5,
                "containshelper",
            ): FunctionTableEntry {
                name: "containshelper",
                arity: 3,
                arg_types: [
                    Custom(
                        "Slice",
                        [
                            Generic(
                                "?_T",
                            ),
                        ],
                    ),
                    Generic(
                        "?_T",
                    ),
                    Int,
                ],
                return_type: Bool,
            },
        },
    },
    usertype_table: Table {
        entries: {
            (
                6,
                "Slice",
            ): UserTypeTableEntry {
                name: "Slice",
                isdefined: true,
                generics: [],
                methods: [
                    FunctionTableEntry {
                        name: "contains",
                        arity: 2,
                        arg_types: [
                            Generic(
                                "?_2c71bd48d95c9b64",
                            ),
                            Generic(
                                "?_T",
                            ),
                        ],
                        return_type: Bool,
                    },
                ],
                kind: Struct {
                    fields: [
                        (
                            "start",
                            Pointer(
                                Generic(
                                    "?_T",
                                ),
                            ),
                        ),
                        (
                            "len",
                            Int,
                        ),
                        (
                            "cap",
                            Int,
                        ),
                    ],
                },
            },
            (
                7,
                "Result",
            ): UserTypeTableEntry {
                name: "Result",
                isdefined: true,
                generics: [],
                methods: [],
                kind: Enum {
                    variants: [
                        Variant(
                            "Ok",
                            [
                                Generic(
                                    "?_T",
                                ),
                            ],
                        ),
                        Variant(
                            "Err",
                            [
                                Generic(
                                    "?_E",
                                ),
                            ],
                        ),
                    ],
                },
            },
            (
                3,
                "Slice",
            ): UserTypeTableEntry {
                name: "Slice",
                isdefined: true,
                generics: [],
                methods: [],
                kind: Struct {
                    fields: [
                        (
                            "start",
                            Pointer(
                                Generic(
                                    "?_T",
                                ),
                            ),
                        ),
                        (
                            "len",
                            Int,
                        ),
                        (
                            "cap",
                            Int,
                        ),
                    ],
                },
            },
            (
                4,
                "Slice",
            ): UserTypeTableEntry {
                name: "Slice",
                isdefined: true,
                generics: [],
                methods: [
                    FunctionTableEntry {
                        name: "len",
                        arity: 1,
                        arg_types: [
                            Generic(
                                "?_2c71bd48d95c9b64",
                            ),
                        ],
                        return_type: Generic(
                            "?_875584f62ed82e73",
                        ),
                    },
                    FunctionTableEntry {
                        name: "new",
                        arity: 1,
                        arg_types: [
                            Generic(
                                "?_735b2f4734cda621",
                            ),
                        ],
                        return_type: Generic(
                            "?_e5e135daabbc9bf2",
                        ),
                    },
                    FunctionTableEntry {
                        name: "get",
                        arity: 2,
                        arg_types: [
                            Generic(
                                "?_2c71bd48d95c9b64",
                            ),
                            Int,
                        ],
                        return_type: Generic(
                            "?_T",
                        ),
                    },
                ],
                kind: Struct {
                    fields: [
                        (
                            "start",
                            Pointer(
                                Generic(
                                    "?_T",
                                ),
                            ),
                        ),
                        (
                            "len",
                            Int,
                        ),
                        (
                            "cap",
                            Int,
                        ),
                    ],
                },
            },
        },
    },
    current_variables: Table {
        entries: {},
    },
    currentfreeid: 7,
}