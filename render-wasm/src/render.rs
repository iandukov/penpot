use skia_safe as skia;
use skia_safe::gpu::{self, gl::FramebufferInfo, DirectContext};

use crate::state::State;

extern "C" {
    pub fn emscripten_GetProcAddress(
        name: *const ::std::os::raw::c_char,
    ) -> *const ::std::os::raw::c_void;
}

pub(crate) struct GpuState {
    pub context: DirectContext,
    framebuffer_info: FramebufferInfo,
}

pub(crate) struct RenderState {
    pub gpu_state: GpuState,
    pub surface: skia::Surface,
}

impl RenderState {
    pub fn new(width: i32, height: i32) -> RenderState {
        let mut gpu_state = create_gpu_state();
        let surface = create_surface(&mut gpu_state, width, height);
        RenderState {
            gpu_state,
            surface,
        }
    }
}

pub(crate) fn init_gl() {
    unsafe {
        gl::load_with(|addr| {
            let addr = std::ffi::CString::new(addr).unwrap();
            emscripten_GetProcAddress(addr.into_raw() as *const _) as *const _
        });
    }
}

/// This needs to be done once per WebGL context.
pub(crate) fn create_gpu_state() -> GpuState {
    let interface = skia_safe::gpu::gl::Interface::new_native().unwrap();
    let context = skia_safe::gpu::direct_contexts::make_gl(interface, None).unwrap();
    let framebuffer_info = {
        let mut fboid: gl::types::GLint = 0;
        unsafe { gl::GetIntegerv(gl::FRAMEBUFFER_BINDING, &mut fboid) };

        FramebufferInfo {
            fboid: fboid.try_into().unwrap(),
            format: skia_safe::gpu::gl::Format::RGBA8.into(),
            protected: skia_safe::gpu::Protected::No,
        }
    };

    GpuState {
        context,
        framebuffer_info,
    }
}

/// Create the Skia surface that will be used for rendering.
pub(crate) fn create_surface(gpu_state: &mut GpuState, width: i32, height: i32) -> skia::Surface {
    let backend_render_target =
        gpu::backend_render_targets::make_gl((width, height), 1, 8, gpu_state.framebuffer_info);

    gpu::surfaces::wrap_backend_render_target(
        &mut gpu_state.context,
        &backend_render_target,
        skia_safe::gpu::SurfaceOrigin::BottomLeft,
        skia_safe::ColorType::RGBA8888,
        None,
        None,
    )
    .unwrap()
}

pub(crate) fn render_rect(surface: &mut skia::Surface, rect: skia::Rect, color: skia::Color) {
    let mut paint = skia::Paint::default();
    paint.set_style(skia::PaintStyle::Fill);
    paint.set_color(color);
    paint.set_anti_alias(true);
    surface.canvas().draw_rect(rect, &paint);
}

// FIXME: Utilizar este método me daba todo el rato el error
// error[E0499]: cannot borrow `*state` as mutable more than once at a time
/*
#[inline]
fn render_shape(state: &mut State, shape: &Shape) {
    let r = skia::Rect::new(
        shape.selrect.x1,
        shape.selrect.y1,
        shape.selrect.x2,
        shape.selrect.y2,
    );

    state.surface.canvas().save();

    let mut matrix = skia::Matrix::new_identity();
    matrix.set_scale_translate(shape.scale(), shape.translation());
    let (skew_x, skew_y) = shape.skew();
    matrix.set_skew_x(skew_x);
    matrix.set_skew_y(skew_y);
    state.surface.canvas().concat(&matrix);
s
    render_rect(&mut state.surface, r, skia::Color::RED);

    state.surface.canvas().restore();
}
*/

pub(crate) fn render_all(state: &mut State) {
    println!("render_all {}", state.display_list.len());
    for id in state.display_list.iter() {
        if let Some(shape) = state.shapes.get(id) {
            println!("Render shape {:?}", shape);

            let r = skia::Rect::new(
                shape.selrect.x1,
                shape.selrect.y1,
                shape.selrect.x2,
                shape.selrect.y2,
            );

            state.render_state.surface.canvas().save();

            let mut matrix = skia::Matrix::new_identity();
            matrix.set_scale_translate(shape.scale(), shape.translation());

            let (skew_x, skew_y) = shape.skew();
            matrix.set_skew_x(skew_x);
            matrix.set_skew_y(skew_y);

            state.render_state.surface.canvas().concat(&matrix);

            render_rect(&mut state.render_state.surface, r, skia::Color::RED);

            state.render_state.surface.canvas().restore();

            // state.render_state.surface.canvas().save();

            // let mut matrix = skia::Matrix::new_identity();
            // matrix.set_scale_translate(shape.scale(), shape.translation());
            // let (skew_x, skew_y) = shape.skew();
            // matrix.set_skew_x(skew_x);
            // matrix.set_skew_y(skew_y);
            // state.render_state.surface.canvas().concat(&matrix);

            // render_rect(&mut state.render_state.surface, r, skia::Color::RED);

            // state.render_state.surface.canvas().restore();
        }
    }
}
