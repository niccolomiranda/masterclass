const PARAMS = {
  uProgress: { value: 0.5 },
  uBaseNoiseIteration: { value: 3 },
  uNoiseDiffusion: { value: 0.84 },
  uNoisePrecision: { value: 2.28 },
  uLightningThickness: { value: 0.4 },
  uLightningPower: { value: 0.04 },
  uLightningDiffusion: { value: 0.03 },
  uColor: 0xc4ff7f,
  uVanishDirection: { value: new THREE.Vector2(-1, -1) },
  uTimeSpeed: 1,
  bloom: {
    radius: 0.11,
    intensity: 5.98,
    luminanceEnabled: true,
    luminanceThreshold: 0.619,
    luminanceSmoothing: 0.63,
    mipmapBlur: true,
    blendFunction: BlendFunction.SCREEN,
  },
};

class App {
  constructor() {
    THREE.ColorManagement.enabled = true;

    this.indexTexture = 0;

    this.tweenParamsCharacter = {
      ease: "power1.inOut",
      duration: 1.1,
    };

    this.tweenParamsBackground = {
      ease: "power1.inOut",
      duration: 1,
      delay: 0.5,
    };

    this.clock = new THREE.Clock();

    this.scene = new THREE.Scene();

    this.camera = new THREE.OrthographicCamera(window.innerWidth / -2, window.innerWidth / 2, window.innerHeight / 2, window.innerHeight / -2, 1, 1000);

    this.camera.position.set(0, 0, 500);

    this.renderer = new THREE.WebGLRenderer({
      canvas: document.getElementById("canvas"),
      precision: "highp",
      powerPreference: "high-performance",
      alpha: true,
      antialias: false,
      stencil: false,
      depth: false,
    });
    this.renderer.useLegacyLights = true;
    this.renderer.outputEncoding = THREE.sRGBEncoding;
    this.renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));

    this.isWebgl2 = this.renderer.capabilities.isWebGL2;
    this.maxSamples = this.renderer.capabilities.maxSamples;

    this.composer = new EffectComposer(this.renderer, { frameBufferType: THREE.HalfFloatType, multisampling: this.isWebgl2 ? Math.min(4, this.maxSamples) : 0 });
    this.renderPass = new RenderPass(this.scene, this.camera);

    if (!this.isWebgl2) {
      this.SMAAEffect = new SMAAEffect({
        blendFunction: BlendFunction.NORMAL,
        preset: SMAAPreset.MEDIUM,
        edgeDetectionMode: EdgeDetectionMode.COLOR,
        predicationMode: PredicationMode.DEPTH,
      });

      const edgeDetectionMaterial = this.SMAAEffect.edgeDetectionMaterial;
      edgeDetectionMaterial.edgeDetectionThreshold = 0.02;
      edgeDetectionMaterial.predicationThreshold = 0.002;
      edgeDetectionMaterial.predicationScale = 1;
      this.SMAAPass = new EffectPass(this.camera, this.SMAAEffect);
    }

    this.bloomEffect = new SelectiveBloomEffect(this.scene, this.camera, PARAMS.bloom);
    this.bloomPass = new EffectPass(this.camera, this.bloomEffect);

    this.composer.addPass(this.renderPass);
    this.composer.addPass(this.bloomPass);

    if (!this.isWebgl2) {
      this.composer.addPass(this.SMAAPass);
    }

    this.loadTexture()
      .then(([textures, texturesBg]) => {
        this.textures = textures;
        this.texturesBg = texturesBg;

        this.initMaterial();
      })
      .catch((error) => {
        console.error(error);
      });

    this.onTick();

    this.onWindowResize();

    this.buttons = document.querySelectorAll(".hero_trigger-img");
    console.log(this.buttons);

    this.buttons.forEach((btn) => {
      const id = parseInt(btn.getAttribute("data-char-id"));

      btn.addEventListener("click", this.onClickBtn.bind(this, id));
    });

    window.addEventListener("resize", this.onWindowResize.bind(this), false);
  }

  onClickBtn(id) {
    if (id === this.indexTexture) return;

    if (id > this.indexTexture) {
      this.onNext(true, id);
    } else {
      this.onPrevious(true, id);
    }
  }

  onSelectNextThumbnail() {}

  onSelectPreviousThumbnail() {}

  initMaterial() {
    const { uProgress, uBaseNoiseIteration, uNoiseDiffusion, uColor, uNoisePrecision, uLightningThickness, uLightningPower, uLightningDiffusion, uVanishDirection } = PARAMS;

    this.material = new THREE.ShaderMaterial({
      uniforms: {
        uTexture: { value: this.textures[this.indexTexture] },
        uProgress,
        uBaseNoiseIteration,
        uNoisePrecision,
        uColor: { value: new THREE.Color(uColor) },
        uNoiseDiffusion,
        uLightningThickness,
        uLightningPower,
        uLightningDiffusion,
        uVanishDirection,
        uTime: { value: 0 },
      },
      vertexShader: `
                varying vec2 vUv;
                varying vec3 vPos;
                void main() {
                    vUv = uv;
                    vPos = position;
                    gl_Position = projectionMatrix * modelViewMatrix * vec4(vPos, 1.0 );
                }
            `,
      fragmentShader: `
                precision highp float;
                #define OCTAVES 10
                #define PI 3.14159265359
                // Description : Array and textureless GLSL 2D/3D/4D simplex
                //               noise functions.
                //      Author : Ian McEwan, Ashima Arts.
                //  Maintainer : ijm
                //     Lastmod : 20110822 (ijm)
                //     License : Copyright (C) 2011 Ashima Arts. All rights reserved.
                //               Distributed under the MIT License. See LICENSE file.
                //               https://github.com/ashima/webgl-noise
                //
                vec3 mod289(vec3 x) {
                  return x - floor(x * (1.0 / 289.0)) * 289.0;
                }
                vec4 mod289(vec4 x) {
                  return x - floor(x * (1.0 / 289.0)) * 289.0;
                }
                vec4 permute(vec4 x) {
                    return mod289(((x*34.0)+1.0)*x);
                }
                vec4 taylorInvSqrt(vec4 r)
                {
                  return 1.79284291400159 - 0.85373472095314 * r;
                }
                float noise3D(vec3 v)
                  {
                  const vec2  C = vec2(1.0/6.0, 1.0/3.0) ;
                  const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);
                // First corner
                  vec3 i  = floor(v + dot(v, C.yyy) );
                  vec3 x0 =   v - i + dot(i, C.xxx) ;
                // Other corners
                  vec3 g = step(x0.yzx, x0.xyz);
                  vec3 l = 1.0 - g;
                  vec3 i1 = min( g.xyz, l.zxy );
                  vec3 i2 = max( g.xyz, l.zxy );
                  //   x0 = x0 - 0.0 + 0.0 * C.xxx;
                  //   x1 = x0 - i1  + 1.0 * C.xxx;
                  //   x2 = x0 - i2  + 2.0 * C.xxx;
                  //   x3 = x0 - 1.0 + 3.0 * C.xxx;
                  vec3 x1 = x0 - i1 + C.xxx;
                  vec3 x2 = x0 - i2 + C.yyy; // 2.0*C.x = 1/3 = C.y
                  vec3 x3 = x0 - D.yyy;      // -1.0+3.0*C.x = -0.5 = -D.y
                // Permutations
                  i = mod289(i);
                  vec4 p = permute( permute( permute(
                            i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
                          + i.y + vec4(0.0, i1.y, i2.y, 1.0 ))
                          + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));
                // Gradients: 7x7 points over a square, mapped onto an octahedron.
                // The ring size 17*17 = 289 is close to a multiple of 49 (49*6 = 294)
                  float n_ = 0.142857142857; // 1.0/7.0
                  vec3  ns = n_ * D.wyz - D.xzx;
                  vec4 j = p - 49.0 * floor(p * ns.z * ns.z);  //  mod(p,7*7)
                  vec4 x_ = floor(j * ns.z);
                  vec4 y_ = floor(j - 7.0 * x_ );    // mod(j,N)
                  vec4 x = x_ *ns.x + ns.yyyy;
                  vec4 y = y_ *ns.x + ns.yyyy;
                  vec4 h = 1.0 - abs(x) - abs(y);
                  vec4 b0 = vec4( x.xy, y.xy );
                  vec4 b1 = vec4( x.zw, y.zw );
                  //vec4 s0 = vec4(lessThan(b0,0.0))*2.0 - 1.0;
                  //vec4 s1 = vec4(lessThan(b1,0.0))*2.0 - 1.0;
                  vec4 s0 = floor(b0)*2.0 + 1.0;
                  vec4 s1 = floor(b1)*2.0 + 1.0;
                  vec4 sh = -step(h, vec4(0.0));
                  vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy ;
                  vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww ;
                  vec3 p0 = vec3(a0.xy,h.x);
                  vec3 p1 = vec3(a0.zw,h.y);
                  vec3 p2 = vec3(a1.xy,h.z);
                  vec3 p3 = vec3(a1.zw,h.w);
                //Normalise gradients
                  vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));
                  p0 *= norm.x;
                  p1 *= norm.y;
                  p2 *= norm.z;
                  p3 *= norm.w;
                // Mix final noise value
                  vec4 m = max(0.6 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
                  m = m * m;
                  return 42.0 * dot( m*m, vec4( dot(p0,x0), dot(p1,x1),
                                                dot(p2,x2), dot(p3,x3) ) );
                  }
                  float mod289(float x) {
                    return x - floor(x * (1.0 / 289.0)) * 289.0; }
                  float permute(float x) {
                      return mod289(((x*34.0)+1.0)*x);
                  }
                  float taylorInvSqrt(float r)
                  {
                    return 1.79284291400159 - 0.85373472095314 * r;
                  }
                  vec4 grad4(float j, vec4 ip)
                    {
                    const vec4 ones = vec4(1.0, 1.0, 1.0, -1.0);
                    vec4 p,s;
                    p.xyz = floor( fract (vec3(j) * ip.xyz) * 7.0) * ip.z - 1.0;
                    p.w = 1.5 - dot(abs(p.xyz), ones.xyz);
                    s = vec4(lessThan(p, vec4(0.0)));
                    p.xyz = p.xyz + (s.xyz*2.0 - 1.0) * s.www;
                    return p;
                    }
                  // (sqrt(5) - 1)/4 = F4, used once below
                  #define F4 0.309016994374947451
                  float noise4D(vec4 v)
                    {
                    const vec4  C = vec4( 0.138196601125011,  // (5 - sqrt(5))/20  G4
                                          0.276393202250021,  // 2 * G4
                                          0.414589803375032,  // 3 * G4
                                        -0.447213595499958); // -1 + 4 * G4
                  // First corner
                    vec4 i  = floor(v + dot(v, vec4(F4)) );
                    vec4 x0 = v -   i + dot(i, C.xxxx);
                  // Other corners
                  // Rank sorting originally contributed by Bill Licea-Kane, AMD (formerly ATI)
                    vec4 i0;
                    vec3 isX = step( x0.yzw, x0.xxx );
                    vec3 isYZ = step( x0.zww, x0.yyz );
                  //  i0.x = dot( isX, vec3( 1.0 ) );
                    i0.x = isX.x + isX.y + isX.z;
                    i0.yzw = 1.0 - isX;
                  //  i0.y += dot( isYZ.xy, vec2( 1.0 ) );
                    i0.y += isYZ.x + isYZ.y;
                    i0.zw += 1.0 - isYZ.xy;
                    i0.z += isYZ.z;
                    i0.w += 1.0 - isYZ.z;
                    // i0 now contains the unique values 0,1,2,3 in each channel
                    vec4 i3 = clamp( i0, 0.0, 1.0 );
                    vec4 i2 = clamp( i0-1.0, 0.0, 1.0 );
                    vec4 i1 = clamp( i0-2.0, 0.0, 1.0 );
                    //  x0 = x0 - 0.0 + 0.0 * C.xxxx
                    //  x1 = x0 - i1  + 1.0 * C.xxxx
                    //  x2 = x0 - i2  + 2.0 * C.xxxx
                    //  x3 = x0 - i3  + 3.0 * C.xxxx
                    //  x4 = x0 - 1.0 + 4.0 * C.xxxx
                    vec4 x1 = x0 - i1 + C.xxxx;
                    vec4 x2 = x0 - i2 + C.yyyy;
                    vec4 x3 = x0 - i3 + C.zzzz;
                    vec4 x4 = x0 + C.wwww;
                  // Permutations
                    i = mod289(i);
                    float j0 = permute( permute( permute( permute(i.w) + i.z) + i.y) + i.x);
                    vec4 j1 = permute( permute( permute( permute (
                              i.w + vec4(i1.w, i2.w, i3.w, 1.0 ))
                            + i.z + vec4(i1.z, i2.z, i3.z, 1.0 ))
                            + i.y + vec4(i1.y, i2.y, i3.y, 1.0 ))
                            + i.x + vec4(i1.x, i2.x, i3.x, 1.0 ));
                  // Gradients: 7x7x6 points over a cube, mapped onto a 4-cross polytope
                  // 7*7*6 = 294, which is close to the ring size 17*17 = 289.
                    vec4 ip = vec4(1.0/294.0, 1.0/49.0, 1.0/7.0, 0.0) ;
                    vec4 p0 = grad4(j0,   ip);
                    vec4 p1 = grad4(j1.x, ip);
                    vec4 p2 = grad4(j1.y, ip);
                    vec4 p3 = grad4(j1.z, ip);
                    vec4 p4 = grad4(j1.w, ip);
                  // Normalise gradients
                    vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));
                    p0 *= norm.x;
                    p1 *= norm.y;
                    p2 *= norm.z;
                    p3 *= norm.w;
                    p4 *= taylorInvSqrt(dot(p4,p4));
                  // Mix contributions from the five corners
                    vec3 m0 = max(0.6 - vec3(dot(x0,x0), dot(x1,x1), dot(x2,x2)), 0.0);
                    vec2 m1 = max(0.6 - vec2(dot(x3,x3), dot(x4,x4)            ), 0.0);
                    m0 = m0 * m0;
                    m1 = m1 * m1;
                    return 49.0 * ( dot(m0*m0, vec3( dot( p0, x0 ), dot( p1, x1 ), dot( p2, x2 )))
                                + dot(m1*m1, vec2( dot( p3, x3 ), dot( p4, x4 ) ) ) ) ;
                    }
                uniform sampler2D uTexture;
                uniform float uProgress;
                uniform float uTime;
                uniform float uBaseNoiseIteration;
                uniform float uNoiseDiffusion;
                uniform float uNoisePrecision;
                uniform float uLightningDiffusion;
                uniform float uLightningThickness;
                uniform float uLightningPower;
                uniform vec2 uVanishDirection;
                uniform vec3 uColor;
                varying vec2 vUv;
                varying vec3 vPos;
              vec2 rotate(vec2 v, float a) {
                float s = sin(a);
                float c = cos(a);
                mat2 m = mat2(c, -s, s, c);
                return m * v;
            }
            float fbm(vec4 pos, float maxIteration) {
                float iterations = 0.;
                float amplitude = 1.;
                float period = 1.;
                for (int i = 0; i < OCTAVES; i++) {
                    if (float(i) > maxIteration) break;
                    period *= uNoisePrecision;
                    amplitude *= 0.9;
                    iterations += noise4D(vec4(pos.xyz * period, pos.w)) * amplitude;
                }
                return (iterations / maxIteration) * 0.5 + 0.5;
            }
            void main() {
              vec4 texture = texture2D(uTexture, vUv);
              if (texture.a == 0.) discard;
              vec3 pos = vPos;
              vec2 vD = uVanishDirection;
              float angle = atan(vD.x, vD.y) + PI / 2.;
              pos.xy = rotate(pos.xy, angle);
              float pX = 0.5 -  pos.x / .95; // .9 = size (by default 1.0)
              float nD = pow(uNoiseDiffusion, 3.);
              float p = smoothstep(pX, pX + nD, (uProgress) * (1. + nD));
              // float noise = 0.0;
              // if (texture.a > 0.0) {
              //   noise = fbm(vec4(pos, uTime * .1 + uProgress), uBaseNoiseIteration);
              // }
              float noise = fbm(vec4(pos, uTime * .1 + uProgress), uBaseNoiseIteration);
              // float pNoise = 0.0;
              // if (texture.a > 0.0) {
              //   pNoise = noise3D(vec3(vUv * noise, p * noise)) * 0.5 + 0.5;
              // }
              float pNoise = noise3D(vec3(vUv * noise, p * noise)) * 0.5 + 0.5;
              float progressNoise = smoothstep(0., 0.3, p - pNoise);
              float maskProgress = smoothstep(.0, uLightningDiffusion, progressNoise);
              vec4 finalColor = mix(texture, vec4(0.), maskProgress);
              vec4 light = vec4(uColor, 1.);
              finalColor = mix(finalColor, light, maskProgress - smoothstep(0., uLightningThickness * 1., progressNoise));
              finalColor = mix(finalColor, vec4(1.), maskProgress - smoothstep(0., uLightningPower * 1., progressNoise));
              if (finalColor.a <= 0.) discard;
              gl_FragColor = finalColor;
            }
            `,
      transparent: true,
    });

    this.materialBg = new THREE.ShaderMaterial({
      uniforms: {
        uTexture: { value: this.texturesBg[this.indexTexture] },
        uProgress: { value: 0 },
        uResolutionEl: {
          value: new THREE.Vector2(),
        },
        uRatio: {
          value: new THREE.Vector2(),
        },
        uTime: { value: 0 },
        uPower: { value: 3 },
        uTextureTo: { value: null },
      },
      vertexShader: `
                varying vec2 vUv;
                varying vec3 vPos;
                void main() {
                    vUv = uv;
                    vPos = position;
                    gl_Position = projectionMatrix * modelViewMatrix * vec4(vPos, 1.0 );
                }
            `,
      fragmentShader: `
                precision highp float;
                varying vec2 vUv;
                uniform sampler2D uTexture;
                uniform sampler2D uTextureTo;
                uniform float uProgress;
                uniform float uTime;
                uniform vec2 uRatio;
                uniform vec2 uResolutionEl;
                uniform float uPower;
                // aspect ratio = ratio de l'image
                // resolution = ratio de la div
                vec2 resizedUv(vec2 inital_uv, vec2 aspect_ratio)
                {
                  vec2 ratio = vec2(
                    min((uResolutionEl.x / uResolutionEl.y) / (aspect_ratio.x / aspect_ratio.y), 1.0),
                    min((uResolutionEl.y / uResolutionEl.x) / (aspect_ratio.y / aspect_ratio.x), 1.0)
                  );
                  vec2 new_uv = vec2(
                    inital_uv.x * ratio.x + (1.0 - ratio.x) * 0.5,
                    inital_uv.y * ratio.y + (1.0 - ratio.y) * 0.5
                  );
                  return new_uv;
                }
                // ColourDistance Transition
                vec4 transition(vec2 p) {
                  vec4 fTex = texture2D(uTexture, p);
                  vec4 tTex = texture2D(uTextureTo, p);
                  float m = step(distance(fTex, tTex), uProgress);
                  return mix(
                    mix(fTex, tTex, m),
                    tTex,
                    pow(uProgress, uPower)
                  );
                }
                void main() {
                  vec2 uv = resizedUv(vUv, uRatio);
                  // vec4 color = transition(uv);
                  vec4 color = mix(texture2D(uTexture, uv), texture2D(uTextureTo, uv), uProgress);
                  gl_FragColor = color;
                }
            `,
      transparent: true,
    });

    this.initMesh();
    this.initBg();
    this.initGUI();
    this.updateBoundingRect();
  }

  initMesh() {
    const geometry = new THREE.PlaneGeometry(1, 1);

    this.mesh = new THREE.Mesh(geometry, this.material);

    this.bloomEffect.selection.add(this.mesh);

    this.scene.add(this.mesh);
  }

  initBg() {
    const geometry = new THREE.PlaneGeometry(1, 1);

    this.bg = new THREE.Mesh(geometry, this.materialBg);
    this.bg.position.z = -1;

    this.scene.add(this.bg);
  }

  onPrevious(selectedMode = false, id = 0) {
    if (this.tl?.isActive() || !this.textures[id] || !this.texturesBg[id]) return;

    let index;

    // Background Animation
    gsap.to(this.materialBg.uniforms.uProgress, {
      value: 1,
      ...this.tweenParamsBackground,
      onStart: () => {
        if (selectedMode) {
          index = id;
        } else {
          index = this.indexTexture === 0 ? this.textures.length - 1 : this.indexTexture - 1;
        }

        this.materialBg.uniforms.uTextureTo.value = this.texturesBg[index];
      },
      onComplete: () => {
        this.materialBg.uniforms.uTexture.value = this.texturesBg[index];
        this.materialBg.uniforms.uProgress.value = 0;
      },
    });

    this.tl?.kill();

    // Character Animation
    this.tl = gsap.timeline().set(this.material.uniforms.uVanishDirection.value, {
      ...this.genRandomVanishDirection(true),
    });

    // Character Animation Disappear
    this.tl
      .to(this.material.uniforms.uProgress, {
        value: 0.95, // by default 1
        ...this.tweenParamsCharacter,
        onComplete: () => {
          if (selectedMode) {
            this.indexTexture = id;
          } else {
            this.indexTexture = this.indexTexture === 0 ? this.textures.length - 1 : this.indexTexture - 1;
          }

          this.material.uniforms.uTexture.value = this.textures[this.indexTexture];
        },
      })
      .set(this.material.uniforms.uVanishDirection.value, {
        ...this.genRandomVanishDirection(false),
      });
    // Character Animation Appear
    this.tl.to(this.material.uniforms.uProgress, {
      value: 0,
      ...this.tweenParamsCharacter,
    });
  }

  onNext(selectedMode = false, id = 0) {
    if (this.tl?.isActive() || !this.textures[id] || !this.texturesBg[id]) return;

    this.tl?.kill();

    let index;

    // Background Animation
    gsap.to(this.materialBg.uniforms.uProgress, {
      value: 1,
      ...this.tweenParamsBackground,
      onStart: () => {
        if (selectedMode) {
          index = id;
        } else {
          index = this.indexTexture === this.textures.length - 1 ? 0 : this.indexTexture + 1;
        }

        this.materialBg.uniforms.uTextureTo.value = this.texturesBg[index];
      },
      onComplete: () => {
        this.materialBg.uniforms.uTexture.value = this.texturesBg[index];
        this.materialBg.uniforms.uProgress.value = 0;
      },
    });

    this.material.uniforms.uVanishDirection.value = this.genRandomVanishDirection(false);

    // Character Animation
    this.tl = gsap.timeline();

    // Disappear Character Animation
    this.tl
      .to(this.material.uniforms.uProgress, {
        value: 0.95, // by default 1
        ...this.tweenParamsCharacter,
        onComplete: () => {
          if (selectedMode) {
            this.indexTexture = id;
          } else {
            this.indexTexture = this.indexTexture === this.textures.length - 1 ? 0 : this.indexTexture + 1;
          }

          this.material.uniforms.uTexture.value = this.textures[this.indexTexture];
        },
      })
      .set(this.material.uniforms.uVanishDirection.value, {
        ...this.genRandomVanishDirection(true),
      });
    // Character Animation Appear
    this.tl
      .to(this.material.uniforms.uProgress, {
        value: 0,
        ...this.tweenParamsCharacter,
      })
      .set(this.material.uniforms.uVanishDirection.value, {
        ...this.genRandomVanishDirection(false),
      });
  }

  genRandomVanishDirection(positive) {
    let x, y;

    if (positive) {
      x = this.genRand(1, 0.5, 1);
      y = this.genRand(1, 0.5, 1);
    } else {
      x = this.genRand(-1, -0.5, 1);
      y = this.genRand(-1, -0.5, 1);
    }

    return new THREE.Vector2(x, y);
  }

  onNextGUI() {
    this.onNext();
  }

  onPreviousGUI() {
    this.onPrevious();
  }

  

  onTick() {
    if (this.mesh) {
      this.material.uniforms.uTime.value += this.clock.getDelta() * PARAMS.uTimeSpeed;
    }

    this.composer.render();

    requestAnimationFrame(this.onTick.bind(this));
  }

  updateBoundingRect() {
    if (!this.mesh) return;

    const naturalWidth = this.textures[0].image.naturalWidth;
    const naturalHeight = this.textures[0].image.naturalHeight;

    const height = window.innerHeight;

    const width = naturalWidth / (naturalHeight / height);

    this.mesh.scale.set(width, height, 1);

    this.bg.scale.set(window.innerWidth, window.innerHeight, 1);

    this.bg.material.uniforms.uResolutionEl.value = new THREE.Vector2(this.bg.scale.x, this.bg.scale.y);

    this.bg.material.uniforms.uRatio.value = new THREE.Vector2(this.texturesBg[this.indexTexture].image.naturalWidth, this.texturesBg[this.indexTexture].image.naturalHeight);
  }

  onWindowResize() {
    this.camera.left = window.innerWidth / -2;
    this.camera.right = window.innerWidth / 2;
    this.camera.top = window.innerHeight / 2;
    this.camera.bottom = window.innerHeight / -2;

    this.camera.updateProjectionMatrix();

    this.composer.setSize(window.innerWidth, window.innerHeight);

    this.updateBoundingRect();
  }

  genRand(min, max, decimalPlaces = 0) {
    const rand = Math.random() * (max - min) + min;
    const power = Math.pow(10, decimalPlaces);
    return Math.floor(rand * power) / power;
  }

  async loadTexture() {
    const loader = new THREE.TextureLoader();

    const imgs = ["https://uploads-ssl.webflow.com/640876ac465a023139e2cd1f/64107241acf34c7f45912388_frame-A.webp", "https://uploads-ssl.webflow.com/640876ac465a023139e2cd1f/64107241add4991560504b55_frame-B.webp"];
    const imgsBg = ["https://uploads-ssl.webflow.com/640876ac465a023139e2cd1f/6410724157a29a2c70bdc4dc_background-A.webp", "https://uploads-ssl.webflow.com/640876ac465a023139e2cd1f/641072410a728d4b107e68e6_background-B.webp"];


    const promises = imgs.map((src) => {
      const url = new URL(src, import.meta.url);

      return loader.loadAsync(url).then((texture) => {
        texture.encoding = THREE.sRGBEncoding;
        return texture;
      });
    });

    const promisesBg = imgsBg.map((src) => {
      const url = new URL(src, import.meta.url);

      return loader.loadAsync(url).then((texture) => {
        texture.encoding = THREE.sRGBEncoding;
        return texture;
      });
    });

    try {
      const textures = await Promise.all(promises);
      const texturesBg = await Promise.all(promisesBg);

      return [textures, texturesBg];
    } catch (error) {
      console.error(error);
    }
  }
}

const app = new App();
