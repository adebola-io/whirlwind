use whirl_ast::Span;

#[derive(Debug)]
pub struct ProjectError {
    pub _type: ProjectErrorType,
    pub span: Option<Span>,
}

#[derive(Debug)]
pub enum ProjectErrorType {
    NamelessModule,
}
