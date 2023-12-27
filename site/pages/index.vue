<template>
   <div class="screen">
      <h1 class="heading-and-icon">
         <svg
            class="whirlwind-icon"
            width="372"
            height="372"
            viewBox="0 0 372 372"
            xmlns="http://www.w3.org/2000/svg"
         >
            <path
               fill-rule="evenodd"
               clip-rule="evenodd"
               d="M25.4794 111.059C25.4794 63.7949 63.7949 25.4794 111.059 25.4794H260.941C308.205 25.4794 346.521 63.7949 346.521 111.059V153.642C346.521 157.664 344.923 161.522 342.078 164.366C332.569 173.875 314.957 167.166 310.987 154.317C295.026 102.653 246.263 65.06 188.585 65.06C117.935 65.06 60.662 121.463 60.662 191.039C60.662 260.615 117.935 317.018 188.585 317.018C188.894 317.018 189.051 317.393 188.832 317.612L164.211 342.233C161.466 344.978 157.743 346.521 153.86 346.521H111.059C63.7949 346.521 25.4794 308.205 25.4794 260.941V111.059ZM282.921 282.921C259.451 306.391 276.073 346.521 309.265 346.521C329.84 346.521 346.521 329.84 346.521 309.265C346.521 276.073 306.391 259.451 282.921 282.921ZM286.054 209.823C283.553 220.477 276.922 229.523 269.184 237.26L240.619 265.825C232.97 273.475 224.042 280.025 213.541 282.629C205.747 284.562 197.586 285.589 189.182 285.589C134.27 285.589 89.7553 241.75 89.7553 187.672C89.7553 133.594 134.27 89.7557 189.182 89.7557C244.094 89.7557 288.609 133.594 288.609 187.672C288.609 195.29 287.726 202.705 286.054 209.823ZM189.182 266.573C233.868 266.573 269.594 230.975 269.594 187.672C269.594 144.37 233.868 108.771 189.182 108.771C144.497 108.771 108.771 144.37 108.771 187.672C108.771 230.975 144.497 266.573 189.182 266.573ZM218.638 156.853C235.895 174.11 235.893 202.603 218 220.496C200.107 238.389 171.615 238.39 154.358 221.133C137.101 203.877 137.102 175.384 154.996 157.491C172.889 139.598 201.381 139.596 218.638 156.853ZM228.48 147.012C251.313 169.845 251.028 207.151 227.842 230.337C204.656 253.523 167.35 253.808 144.516 230.975C121.683 208.141 121.968 170.835 145.154 147.649C168.34 124.464 205.646 124.178 228.48 147.012Z"
            />
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
      rgba(0, 194, 255, 0.17) 100%
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
