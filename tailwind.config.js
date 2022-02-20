module.exports = {
	content: ["./docs/**/*.{html,js}"],
	theme: {
		extend: {
			colors: {
				"notion-light": "#ffffff",
				"notion-dark": "#2F3437",
				"notion-subtle-light": "#FBFAF9",
				"notion-subtle-dark": "#3F4447",
			},
			screens: {
				// "device-sm": { raw: "(min-device-width: 640px)" },
			},
			boxShadow: {
				inner: "0 1px 1px 0 rgba(255, 255, 255, 0.3) inset",
			},
		},
	},
	plugins: [require("@tailwindcss/forms")],
}
