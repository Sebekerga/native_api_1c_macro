use std::sync::Arc;

use native_api_1c::{native_api_1c_core::ffi::connection::Connection, native_api_1c_macro::AddIn};

#[derive(AddIn)]
pub struct MyAddIn {
    #[add_in_con]
    connection: Arc<Option<&'static Connection>>, // Arc для возможности многопоточности

    #[add_in_prop(name = "MyProp", name_ru = "МоеСвойство", readable, writable)]
    pub some_prop: i32,
    #[add_in_prop(name = "ProtectedProp", name_ru = "ЗащищенноеСвойство", readable)]
    pub protected_prop: i32,
    #[add_in_func(name = "MyFunction", name_ru = "МояФункция")]
    #[arg(Int)]
    #[returns(Int, result)]
    pub my_function: fn(&Self, i32) -> Result<i32, ()>,

    private_field: i32,
}

impl MyAddIn {
    pub fn new() -> Self {
        Self {
            connection: Arc::new(None),
            some_prop: 0,
            protected_prop: 50,
            my_function: Self::my_function,
            private_field: 100,
        }
    }

    fn my_function(&self, arg: i32) -> Result<i32, ()> {
        Ok(self.protected_prop + self.some_prop + arg + self.private_field)
    }
}

fn main() {
    let _add_in = MyAddIn::new();
}
