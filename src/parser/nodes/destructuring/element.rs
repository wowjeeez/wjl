use crate::parser::ast::PartialAstIr;

#[derive(PartialEq, Debug, Clone)]
pub struct DestructuringElement {
    pub(crate) name: Option<String>, //option cuz it can be a nested array destructuring which doesnt have a name
    pub(crate) default_value: Option<Vec<PartialAstIr>>,
    //can either be an ARRAY_VARIABLE_DESTRUCTURING or an OBJECT_VARIABLE_DESTRUCTURING
    pub(crate) children: Option<PartialAstIr>,
    pub(crate) actual_binding_name: Option<String>
}

