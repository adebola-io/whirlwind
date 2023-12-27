// https://nuxt.com/docs/api/configuration/nuxt-config
export default defineNuxtConfig({
   app: {
      head: {
         title: "Whirlwind | A Language for Building Resilient Software.",
         link: [
            {
               rel: "preconnect",
               href: "https://fonts.googleapis.com",
            },
            {
               rel: "preconnect",
               href: "https://fonts.gstatic.com",
               crossorigin: "",
            },
            {
               rel: "stylesheet",
               href: "https://fonts.googleapis.com/css2?family=Lexend+Deca:wght@100;200;300;400;500;600;700;800;900&family=Lexend:wght@200;300;400;500;600;700;800;900&display=swap",
            },
         ],
      },
   },
   devtools: { enabled: true },
});
