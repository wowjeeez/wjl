use std::fmt::{Debug, Display};
use crate::parser::ast::PartialAstIr;
use crate::parser::nodes::element::DestructuringElement;

fn str_destruct_elem(elem: &DestructuringElement) -> String {
    format!("name: {:?}, default: {:?}, children: {:?}", elem.name, str_option(elem.default_value.as_ref().map(|x|x.iter()
        .map(|node| node.to_debug_str()))), elem.children.as_ref()
        .map(|x|x.to_debug_str()))
}

fn str_option<T: Debug>(elem: Option<T>) -> String {
    if elem.is_some() {
        return format!("{:?}", elem.unwrap())
    }
    return format!("None")
}

impl PartialAstIr {
    pub fn to_debug_str(&self) -> String {
        match self {
            PartialAstIr::VAR_DECL(var) => format!("{{VAR_DECL name: {}, modifiers: {:?}, visibility: {:?}, decl_type: {:?}, expl_type: {:?}, inf_type: {:?}, initializer: {:?}}}", var.name.to_debug_str(), var.modifiers, var.visibility, var.decltype, str_option(var.explicit_type.as_ref()), var.inferred_type, var.initializer.as_ref().map(|x|x.iter().map(|x|x.to_debug_str()).collect::<Vec<String>>())),
            PartialAstIr::IDENT_REF(path) => format!("{{IDENT_REF: {:?}}}", path),
            PartialAstIr::ARRAY_VARIABLE_DESTRUCTURING(stmt) => format!("{{ARRAY_DESTRUCT: {:?}}}", stmt.0.iter().map(str_destruct_elem)),
            PartialAstIr::OBJECT_VARIABLE_DESTRUCTURING(stmt) => format!("{{OBJECT_DESTRUCT: {:?}}}", stmt.0.iter().map(str_destruct_elem)),
            PartialAstIr::TYPE_ANNOTATION(typ) => format!("{{TYPE_ANNOTATION: {:?}}}", typ.iter().map(|x| x.to_debug_str()).collect::<Vec<String>>().join(", ")),
            PartialAstIr::PAREN_EXPR(paren) => format!("{{PAREN_EXPR: {:?}}}", paren.iter().map(|x|x.to_debug_str()).collect::<Vec<String>>().join(", ")),
            PartialAstIr::INT(i) => format!("{{INT: {}}}", i),
            PartialAstIr::FLOAT(f) => format!("{{FLOAT: {}}}", f),
            PartialAstIr::_NONCE => "{{NONCE}}".to_string(),
            PartialAstIr::ARBIT_BLOCK(block) => format!("{{ARBITRARY_BLOCK: {:?}}}", block.iter().map(|x| x.to_debug_str()).collect::<Vec<String>>().join(", ")),
            PartialAstIr::ARRAY_DECL(node) => format!("{{ARRAY_DECL: {:?}}}", node.0.iter().map(|x|x.to_debug_str()).collect::<Vec<String>>().join(", ")),
            PartialAstIr::LOGICAL_OP(op) => format!("{{LOGICAL OPERATOR: {:?}}}", op),
            PartialAstIr::GENERIC_DECL(decl) => format!("{{GENERIC_DECL: {:?}}}", decl),
            PartialAstIr::GENERIC_ARG(arg) => format!("{{GENERIC_ARG: {:?}}}", arg)
        }
    }
}

pub fn dbg_vec(v: Vec<PartialAstIr>) -> Vec<String> {
    let vec = v.iter().map(|x|x.to_debug_str()).collect::<Vec<String>>();
    vec.iter().map(|x|x.replace("\\", "").replace("\"", "")).collect::<Vec<String>>()
}

pub trait NodeDebugPrint : Debug { //TODO! impl this
    fn dbg_string(&self) -> String {
        format!("{:?}", self)
    }
}