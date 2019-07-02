pub enum Node {
    Script(Vec<Node>),
    DebuggerStatement,
    EmptyStatement,
}
