<template>
   <div class="screen">
      <h1 class="heading-and-icon">
         <svg
            width="376"
            height="368"
            viewBox="0 0 376 368"
            fill="none"
            xmlns="http://www.w3.org/2000/svg"
         >
            <rect
               x="11"
               y="11"
               width="354"
               height="346"
               rx="20.2742"
               stroke="#A6A6A6"
               stroke-width="22"
            />
            <circle cx="260.5" cy="252.5" r="53.5" fill="#A6A6A6" />
         </svg>
         <span class="text">Whirlwind.</span>
      </h1>
      <p class="first-paragraph">Oh..hi! You're early.</p>
      <p class="second-paragraph">
         Thing is, Whirlwind is still (aggressively) being built.
         <br />
         No worries, you'll be the first to see it once it's ready. :&rpar;
         <br />
         <br />
         <span class="help-prompt">
            In the meantime, you can check out its source, and contribute to its
            development
            <a
               target="_blank"
               rel="noreferrer"
               href="https://github.com/adebola-io/whirlwind"
               >here</a
            >.
         </span>
      </p>
   </div>
</template>

<style>
body {
   height: 100vh;
   margin: 0;
   font-family: Poppins, sans-serif;
   font-size: 1.3vh;
   color: white;
}
@media (aspect-ratio >= 0.8) {
   body {
      font-size: 0.8vw;
   }
}
.screen {
   position: relative;
   display: flex;
   justify-content: center;
   align-items: center;
   flex-direction: column;
   height: 100vh;
   width: 84vw;
   padding: 0 8vw;
   background-color: black;
}
.screen::before {
   position: absolute;
   display: block;
   content: "";
   width: 100%;
   height: 100%;
   background: linear-gradient(
      180deg,
      rgba(0, 0, 0, 0) 0%,
      rgba(0, 44, 57, 0.17) 100%
   );
   animation: fade-in 900ms;
}
.heading-and-icon {
   position: relative;
   display: flex;
   align-items: center;
   justify-content: center;
   flex-direction: column;
   gap: 1vh;
   font-size: 3.8vh;
   font-family: Lexend, Lato, "Segoe UI", Tahoma, Geneva, Verdana, sans-serif;
   text-decoration: underline;
   transition-duration: 500ms;
   animation: come-up 500ms;
}
@keyframes come-up {
   from {
      opacity: 0;
      transform: translateY(20%);
   }
}
.whirlwind-icon {
   height: 10vh;
   width: auto;
   aspect-ratio: 1;
}
@media (aspect-ratio>= 0.8) {
   .heading-and-icon {
      flex-direction: row;
      font-size: 7vh;
      text-align: right;
      gap: 0.7vw;
      text-decoration: none;
      margin-bottom: 0;
   }
   .whirlwind-icon {
      height: 10vh;
   }
}
.whirlwind-icon path {
   fill: white;
}
p {
   position: relative;
   width: 100%;
   transition-duration: 500ms;
   animation: fade-in 700ms;
}
@keyframes fade-in {
   from {
      opacity: 0;
   }
}
.first-paragraph {
   text-align: left;
}
.second-paragraph {
   text-align: right;
}
.help-prompt a {
   text-decoration: none;
   color: #a7a5a5;
   transition-duration: 500ms;
}
.help-prompt a:hover {
   text-decoration: underline;
   color: rgb(218, 218, 218);
}
@media (aspect-ratio >= 0.6) {
   .help-prompt {
      display: inline-block;
      width: 60%;
   }
}
@media (aspect-ratio >= 0.8) {
   .first-paragraph,
   .second-paragraph {
      text-align: center;
      margin-bottom: 0;
   }
}
</style>
