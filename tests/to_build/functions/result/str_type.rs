use std::sync::Arc;

use native_api_1c::native_api_1c_core::ffi::connection::Connection;
use native_api_1c_macro::AddIn;

#[derive(AddIn)]
pub struct MyAddIn {
    #[add_in_con]
    connection: Arc<Option<&'static Connection>>,

    #[add_in_func(name = "MyFunction", name_ru = "МояФункция")]
    #[arg(Str)]
    #[returns(Str, result)]
    pub my_function: fn(&Self, String) -> Result<String, ()>,
}

impl MyAddIn {
    pub fn new() -> Self {
        Self {
            connection: Arc::new(None),
            my_function: Self::my_function,
        }
    }

    fn my_function(&self, arg: String) -> Result<String, ()> {
        Ok(arg)
    }
}

fn main() {
    let _add_in = MyAddIn::new();
}
