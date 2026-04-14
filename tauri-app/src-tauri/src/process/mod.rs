pub mod runner;

/// Build a `std::process::Command` that does NOT pop up a console window on
/// Windows. Without this, every subprocess spawned by the GUI app (Rscript,
/// quarto, taskkill, which/where, open/xdg-open/explorer, etc.) inherits a
/// console and flashes a visible terminal — very disruptive in a GUI app.
///
/// On non-Windows platforms this is equivalent to `Command::new`. Use it
/// everywhere we spawn a child process from the Tauri backend.
pub fn new_command<S: AsRef<std::ffi::OsStr>>(program: S) -> std::process::Command {
    #[allow(unused_mut)]
    let mut cmd = std::process::Command::new(program);
    #[cfg(windows)]
    {
        use std::os::windows::process::CommandExt;
        const CREATE_NO_WINDOW: u32 = 0x0800_0000;
        cmd.creation_flags(CREATE_NO_WINDOW);
    }
    cmd
}
