use ort::{
  Result,
  session::Session,
  session::builder::GraphOptimizationLevel,
};

#[tokio::main]
async fn main() -> Result<()> {
  let model_id = "onnx-community/chatterbox-multilingual-ONNX";
  let onnx_filename = "onnx/conditional_decoder.onnx";
  let onnx_data_filename = "onnx/conditional_decoder.onnx_data";

  let cache_api = hf_hub::Cache::from_env();
  let api = hf_hub::api::tokio::Api::new().unwrap();

  let conditional_model = match cache_api.model(String::from(model_id)).get(onnx_filename) {
    Some(p) => p,
    None => {
      api.model(String::from(model_id)).get(onnx_filename).await.unwrap()
    }
  };

  let _ = match cache_api.model(String::from(model_id)).get(onnx_data_filename) {
    Some(p) => p,
    None => {
      api.model(String::from(model_id)).get(onnx_data_filename).await.unwrap()
    }
  };

  let _ = Session::builder()?
    .with_optimization_level(GraphOptimizationLevel::Level3)?
    .commit_from_file(conditional_model)?;

  Ok(())
}
