// This allocation should never be detected as a leak.
const FOO: &&u32 = &&42;

fn test() {
    let _ = *FOO;
}
