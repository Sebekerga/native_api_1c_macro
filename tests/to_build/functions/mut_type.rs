use std::sync::Arc;

use native_api_1c::native_api_1c_core::ffi::connection::Connection;
use native_api_1c_macro::AddIn;

#[derive(AddIn)]
pub struct MyAddIn {
    #[add_in_con]
    connection: Arc<Option<&'static Connection>>,

    #[add_in_func(name = "MyFunction", name_ru = "МояФункция")]
    #[arg(Str, as_out)]
    #[returns(Str)]
    pub my_function: fn(&Self, &mut String) -> String,

    #[add_in_func(name = "MyFunction2", name_ru = "МояФункция2")]
    #[arg(Str, as_out)]
    #[returns(Str)]
    pub my_function2: fn(&mut Self, &mut String) -> String,
}

impl MyAddIn {
    pub fn new() -> Self {
        Self {
            connection: Arc::new(None),
            my_function: Self::my_function,
            my_function2: Self::my_function2,
        }
    }

    fn my_function(&self, arg: &mut String) -> String {
        *arg = "new string".into();
        arg
    }

    fn my_function2(&mut self, arg: &mut String) -> String{
        *arg = "new string".into();
        arg
    }
}

fn main() {
    let _add_in = MyAddIn::new();
}
