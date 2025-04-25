use common::span::Span;
use crate::expressions::Expression;
use crate::function_decl::FunctionDeclaration;
use crate::model::Visibility;
use crate::variable::VariableDeclaration;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ClassDeclaration {
    name: Span<Expression>,
    visibility: Option<Span<Visibility>>,
    parent: Option<Span<Expression>>,
    implements: Vec<Span<Expression>>,
    constructor: Option<Span<FunctionDeclaration>>,
    destructor: Option<Span<FunctionDeclaration>>,
    methods: Vec<Span<FunctionDeclaration>>,
    properties: Vec<Span<VariableDeclaration>>,
    class_type: Span<ClassType>,
    _ordering: Vec<Ordering>
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum Ordering {
    CTOR,
    DTOR,
    PROPERTY(usize),
    METHOD(usize),
}


#[derive(Debug, Clone, Eq, PartialEq)]
enum ClassType {
    INTERFACE,
    CLASS,
    ABSTRACT_CLASS,
    ABSTRACT_INTERFACE
}