def _shellcheck_test_implementation(ctx):
    args = ctx.actions.args()
    args.add_all(ctx.files.src)
    args.use_param_file("@%s", use_always = True)

    args_file = ctx.actions.declare_file("{}@shellcheck.params".format(ctx.label.name))
    ctx.actions.write(args_file, args)

    ctx.actions.write(
        output = ctx.outputs.bin,
        content = "\n".join([
            "#!/usr/bin/env bash",
            "{shellcheck} $(< {args_file})",
        ]).format(
            shellcheck = ctx.executable._shellcheck.path,
            args_file = args_file.short_path,
        ),
        is_executable = True,
    )

    default_info = DefaultInfo(
        executable = ctx.outputs.bin,
        files = depset(),
        runfiles = ctx.runfiles(
            collect_data = True,
            collect_default = True,
            files = [args_file] + ctx.files.src + ctx.files._shellcheck,
        ),
    )

    return [default_info]

shellcheck_test = rule(
    attrs = {
        "src": attr.label_list(
            allow_files = True,
        ),
        "_shellcheck": attr.label(
            cfg = "host",
            default = "@shellcheck//:bin/shellcheck",
            allow_files = True,
            executable = True,
        ),
    },
    outputs = {
        "bin": "%{name}-bin",
    },
    test = True,
    implementation = _shellcheck_test_implementation,
)
