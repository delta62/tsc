pub enum Node {
    BindingIdentifier(String),
    DebuggerStatement,
    EmptyStatement,
    Script(Vec<Node>),
}
