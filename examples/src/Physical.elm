module Physical exposing (physical)

import GLSL exposing (..)


physical isTexture =
    let
        color =
            if isTexture then
                s "texture2D(" >> uniform "sampler2D" "color" >> s ", uv.yx).rgb"

            else
                uniform "vec3" "color"

        uv =
            if isTexture then
                \x ->
                    x
                        |> s "// TODO: if we had the uv map in the mesh, this would be all it takes\n"
                        |> s "// vec2 uv = interpolatedUV;\n"
                        |> s "//\n"
                        |> s "// but for now, lets calculate uv maps via this formula for a sphere:\n"
                        |> s "// https://en.wikipedia.org/wiki/UV_mapping#Finding_UV_on_a_sphere\n"
                        |> (s "vec3 iUV = normalize(" >> varying "vec3" "interpolatedUV" >> s ");")
                        |> s "vec2 uv = vec2((atan(iUV.z, iUV.x) / 3.1415926 + 1.0) * 0.5, 0.5 - (asin(iUV.y) / 3.1415926));"

            else
                s ""

        roughness =
            if isTexture then
                s "float r = texture2D(" >> uniform "sampler2D" "roughness" >> s ", uv).r;"

            else
                uniform "float" "roughness"

        metallic =
            if isTexture then
                s "texture2D(" >> uniform "sampler2D" "metallic" >> s ", uv).r"

            else
                uniform "float" "metallic"
    in
    start
        |> define """
uniform mat4 sceneProperties;
uniform mat4 ambientLighting;
uniform mat4 viewMatrix;
uniform mat4 lights12;
uniform mat4 lights34;
uniform mat4 lights56;
uniform mat4 lights78;

varying vec3 interpolatedPosition;
varying vec3 interpolatedNormal;
"""
        |> define positiveDotProduct
        |> define gammaCorrect
        |> define toSrgb
        |> define getDirectionToLightAndNormalIlluminance
        |> define safeQuotient
        |> define specularD
        |> define g1
        |> define specularG
        |> define fresnelColor
        |> define brdf
        |> define vndf
        |> define probabilityDensity
        |> define sampleFacetNormal
        |> define cloudyLuminance
        |> define overcastSpecularSample
        |> define specularLightDirection
        |> define ambientColor
        |> define litColor
        |> s "vec3 normalDirection = normalize(interpolatedNormal);"
        |> s "float projectionType = sceneProperties[1][3];"
        |> s "vec3 directionToCamera = vec3(0.0, 0.0, 0.0);"
        |> s "if (projectionType == 0.0) {"
        |> s "    vec3 cameraPoint = sceneProperties[1].xyz;"
        |> s "    directionToCamera = normalize(cameraPoint - interpolatedPosition);"
        |> s "} else {"
        |> s "    directionToCamera = sceneProperties[1].xyz;"
        |> s "}"
        |> s "float dotNV = positiveDotProduct(normalDirection, directionToCamera);"
        |> uv
        |> (s "float m = " >> metallic >> s ";")
        |> s "float nonmetallic = 1.0 - m;"
        |> (s "vec3 diffColor = " >> color >> s ";")
        |> s "vec3 diffuseBaseColor = nonmetallic * 0.96 * diffColor;"
        |> s "vec3 specularBaseColor = nonmetallic * 0.04 * vec3(1.0, 1.0, 1.0) + m * diffColor;"
        |> s "vec3 linearColor = vec3(0.0, 0.0, 0.0);"
        |> roughness
        |> s "float alpha = r * r;"
        |> s "float alphaSquared = alpha * alpha;"
        |> s "vec3 color0 = ambientColor(normalDirection, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alpha, alphaSquared);"
        |> s "vec3 color1 = litColor(lights12[0], lights12[1], normalDirection, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alphaSquared);"
        |> s "vec3 color2 = litColor(lights12[2], lights12[3], normalDirection, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alphaSquared);"
        |> s "vec3 color3 = litColor(lights34[0], lights34[1], normalDirection, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alphaSquared);"
        |> s "vec3 color4 = litColor(lights34[2], lights34[3], normalDirection, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alphaSquared);"
        |> s "vec3 color5 = litColor(lights56[0], lights56[1], normalDirection, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alphaSquared);"
        |> s "vec3 color6 = litColor(lights56[2], lights56[3], normalDirection, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alphaSquared);"
        |> s "vec3 color7 = litColor(lights78[0], lights78[1], normalDirection, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alphaSquared);"
        |> s "vec3 color8 = litColor(lights78[2], lights78[3], normalDirection, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alphaSquared);"
        |> s "gl_FragColor = toSrgb(color0 + color1 + color2 + color3 + color4 + color5 + color6 + color7 + color8);"


positiveDotProduct =
    """
float positiveDotProduct(vec3 v1, vec3 v2) {
    return clamp(dot(v1, v2), 0.0, 1.0);
}
"""


gammaCorrect =
    """
float gammaCorrect(float u) {
    if (u <= 0.0031308) {
        return 12.92 * u;
    } else {
        return 1.055 * pow(u, 1.0 / 2.4) - 0.055;
    }
}
"""


toSrgb =
    """
vec4 toSrgb(vec3 linearColor) {
    vec3 referenceWhite = sceneProperties[2].rgb;
    float red = gammaCorrect(linearColor.r / referenceWhite.r);
    float green = gammaCorrect(linearColor.g / referenceWhite.g);
    float blue = gammaCorrect(linearColor.b / referenceWhite.b);
    return vec4(red, green, blue, 1.0);
}
"""


getDirectionToLightAndNormalIlluminance =
    """
void getDirectionToLightAndNormalIlluminance(vec4 xyz_type, vec4 rgb_radius, out vec3 directionToLight, out vec3 normalIlluminance) {
    float lightType = xyz_type.w;
    if (lightType == 1.0) {
        directionToLight = xyz_type.xyz;
        normalIlluminance = rgb_radius.rgb;
    } else if (lightType == 2.0) {
        vec3 lightPosition = xyz_type.xyz;
        vec3 displacement = lightPosition - interpolatedPosition;
        float distance = length(displacement);
        directionToLight = displacement / distance;
        normalIlluminance = rgb_radius.rgb / (4.0 * 3.14159265359 * distance * distance);
    }
}
"""


safeQuotient =
    """
float safeQuotient(float numerator, float denominator) {
    if (denominator == 0.0) {
        return 0.0;
    } else {
        return numerator / denominator;
    }
}
"""


specularD =
    """
float specularD(float alphaSquared, float dotNHSquared) {
    float tmp = dotNHSquared * (alphaSquared - 1.0) + 1.0;
    return alphaSquared / (3.14159265359 * tmp * tmp);
}
"""


g1 =
    """
float g1(float dotNV, float alphaSquared) {
    return safeQuotient(2.0 * dotNV, dotNV + sqrt(alphaSquared + (1.0 - alphaSquared) * dotNV * dotNV));
}
"""


specularG =
    """
float specularG(float dotNL, float dotNV, float alphaSquared) {
    return g1(dotNV, alphaSquared) * g1(dotNL, alphaSquared);
}
"""


fresnelColor =
    """
vec3 fresnelColor(vec3 specularBaseColor, float dotVH) {
    vec3 one = vec3(1.0, 1.0, 1.0);
    float scale = exp2((-5.55473 * dotVH - 6.98316) * dotVH);
    return specularBaseColor + (one - specularBaseColor) * scale;
}
"""


brdf =
    """
vec3 brdf(vec3 normalDirection, vec3 directionToCamera, vec3 directionToLight, float alphaSquared, float dotNV, float dotNL, vec3 specularBaseColor, vec3 normalIlluminance) {
    vec3 halfDirection = normalize(directionToCamera + directionToLight);
    float dotVH = positiveDotProduct(directionToCamera, halfDirection);
    float dotNH = positiveDotProduct(normalDirection, halfDirection);
    float dotNHSquared = dotNH * dotNH;
    float d = specularD(alphaSquared, dotNHSquared);
    float g = specularG(dotNL, dotNV, alphaSquared);
    vec3 f = fresnelColor(specularBaseColor, dotVH);
    return safeQuotient(d * g, 4.0 * dotNL * dotNV) * f;
}
"""


vndf =
    """
float vndf(float dotNV, float alphaSquared, float dotNH) {
    return safeQuotient(g1(dotNV, alphaSquared) * dotNV * specularD(alphaSquared, dotNH * dotNH), dotNV);
}
"""


probabilityDensity =
    """
float probabilityDensity(float dotNV, float alphaSquared, float dotNH, float dotVH) {
    return safeQuotient(vndf(dotNV, alphaSquared, dotNH), 4.0 * dotVH);
}
"""


sampleFacetNormal =
    """
vec3 sampleFacetNormal(float t1, float t2, vec3 vH, vec3 vT1, vec3 vT2, float s, float alpha) {
    t2 = (1.0 - s) * sqrt(1.0 - t1 * t1) + s * t2;
    vec3 vNh = t1 * vT1 + t2 * vT2 + sqrt(max(0.0, 1.0 - t1 * t1 - t2 * t2)) * vH;
    return normalize(vec3(alpha * vNh.x, alpha * vNh.y, max(0.0, vNh.z)));
}
"""


cloudyLuminance =
    """
vec3 cloudyLuminance(vec3 zenithLuminance, vec3 localZenithDirection, vec3 localLightDirection) {
    float sinElevation = dot(localLightDirection, localZenithDirection);
    return zenithLuminance * ((1.0 + sinElevation) / 2.0);
}
"""


overcastSpecularSample =
    """
vec3 overcastSpecularSample(
    vec3 zenithLuminance,
    vec3 localZenithDirection,
    vec3 localViewDirection,
    vec3 localLightDirection,
    vec3 localHalfDirection,
    float alphaSquared,
    vec3 specularBaseColor
) {
    vec3 luminance = cloudyLuminance(zenithLuminance, localZenithDirection, localLightDirection);
    float dotVH = positiveDotProduct(localViewDirection, localHalfDirection);
    float dotNL = localLightDirection.z;
    return luminance * (fresnelColor(specularBaseColor, dotVH) * g1(dotNL, alphaSquared));
    return luminance * (fresnelColor(specularBaseColor, dotVH) * g1(dotNL, alphaSquared));
}
"""


specularLightDirection =
    """
vec3 specularLightDirection(vec3 v, vec3 h) {
    return (2.0 * dot(v, h)) * h - v;
}
"""


ambientColor =
    """
vec3 ambientColor(
    vec3 normalDirection,
    vec3 directionToCamera,
    float dotNV,
    vec3 diffuseBaseColor,
    vec3 specularBaseColor,
    float alpha,
    float alphaSquared
) {
    float ambientType = ambientLighting[0][3];
    if (ambientType == 0.0) {
        return vec3(0.0, 0.0, 0.0);
    }
    if (ambientType == 1.0) {
        vec3 zenithDirection = ambientLighting[0].xyz;
        vec3 zenithLuminance = ambientLighting[1].rgb;
        vec3 crossProduct = cross(normalDirection, directionToCamera);
        float crossMagnitude = length(crossProduct);
        vec3 xDirection = vec3(0.0, 0.0, 0.0);
        vec3 yDirection = vec3(0.0, 0.0, 0.0);
        if (crossMagnitude > 1.0e-6) {
            yDirection = (1.0 / crossMagnitude) * crossProduct;
            xDirection = cross(yDirection, normalDirection);
        } else {
            vec3 viewY = vec3(viewMatrix[0][1], viewMatrix[1][1], viewMatrix[2][1]);
            xDirection = normalize(cross(viewY, normalDirection));
            yDirection = cross(normalDirection, xDirection);
        }
        float localViewX = dot(directionToCamera, xDirection);
        float localViewZ = dot(directionToCamera, normalDirection);
        vec3 localViewDirection = vec3(localViewX, 0, localViewZ);
        float localZenithX = dot(zenithDirection, xDirection);
        float localZenithY = dot(zenithDirection, yDirection);
        float localZenithZ = dot(zenithDirection, normalDirection);
        vec3 localZenithDirection = vec3(localZenithX, localZenithY, localZenithZ);
        vec3 vH = normalize(vec3(alpha * localViewX, 0.0, localViewZ));
        vec3 vT1 = vec3(0.0, 1.0, 0.0);
        vec3 vT2 = cross(vH, vT1);
        float s = 0.5 * (1.0 + vH.z);
        
        vec3 localHalfDirection = vec3(0.0, 0.0, 0.0);
        vec3 localLightDirection = vec3(0.0, 0.0, 0.0);
        float numSamples = 13.0;
        
        vec3 specularSum = vec3(0.0, 0.0, 0.0);
        localHalfDirection = sampleFacetNormal(0.000000, 0.000000, vH, vT1, vT2, s, alpha);
        localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
        specularSum += overcastSpecularSample(zenithLuminance, localZenithDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
        
        localHalfDirection = sampleFacetNormal(0.448762, 0.000000, vH, vT1, vT2, s, alpha);
        localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
        specularSum += overcastSpecularSample(zenithLuminance, localZenithDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
        localHalfDirection = sampleFacetNormal(0.000000, 0.448762, vH, vT1, vT2, s, alpha);
        localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
        specularSum += overcastSpecularSample(zenithLuminance, localZenithDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
        
        localHalfDirection = sampleFacetNormal(-0.448762, 0.000000, vH, vT1, vT2, s, alpha);
        localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
        specularSum += overcastSpecularSample(zenithLuminance, localZenithDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
        
        localHalfDirection = sampleFacetNormal(0.000000, -0.448762, vH, vT1, vT2, s, alpha);
        localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
        specularSum += overcastSpecularSample(zenithLuminance, localZenithDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
        
        localHalfDirection = sampleFacetNormal(0.748423, 0.310007, vH, vT1, vT2, s, alpha);
        localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
        specularSum += overcastSpecularSample(zenithLuminance, localZenithDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
        
        localHalfDirection = sampleFacetNormal(0.310007, 0.748423, vH, vT1, vT2, s, alpha);
        localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
        specularSum += overcastSpecularSample(zenithLuminance, localZenithDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
        
        localHalfDirection = sampleFacetNormal(-0.310007, 0.748423, vH, vT1, vT2, s, alpha);
        localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
        specularSum += overcastSpecularSample(zenithLuminance, localZenithDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
        
        localHalfDirection = sampleFacetNormal(-0.748423, 0.310007, vH, vT1, vT2, s, alpha);
        localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
        specularSum += overcastSpecularSample(zenithLuminance, localZenithDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
        
        localHalfDirection = sampleFacetNormal(-0.748423, -0.310007, vH, vT1, vT2, s, alpha);
        localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
        specularSum += overcastSpecularSample(zenithLuminance, localZenithDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
        
        localHalfDirection = sampleFacetNormal(-0.310007, -0.748423, vH, vT1, vT2, s, alpha);
        localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
        specularSum += overcastSpecularSample(zenithLuminance, localZenithDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
        
        localHalfDirection = sampleFacetNormal(0.310007, -0.748423, vH, vT1, vT2, s, alpha);
        localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
        specularSum += overcastSpecularSample(zenithLuminance, localZenithDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
        
        localHalfDirection = sampleFacetNormal(0.748423, -0.310007, vH, vT1, vT2, s, alpha);
        localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
        specularSum += overcastSpecularSample(zenithLuminance, localZenithDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
        
        vec3 diffuseSum = vec3(0.0, 0.0, 0.0);
        localLightDirection = vec3(0.000000, 0.000000, 1.000000);
        diffuseSum += cloudyLuminance(zenithLuminance, localZenithDirection, localLightDirection) * localLightDirection.z;
        localLightDirection = vec3(0.606266, 0.000000, 0.795262);
        diffuseSum += cloudyLuminance(zenithLuminance, localZenithDirection, localLightDirection) * localLightDirection.z;
        
        localLightDirection = vec3(0.000000, 0.606266, 0.795262);
        diffuseSum += cloudyLuminance(zenithLuminance, localZenithDirection, localLightDirection) * localLightDirection.z;
        
        localLightDirection = vec3(-0.606266, 0.000000, 0.795262);
        diffuseSum += cloudyLuminance(zenithLuminance, localZenithDirection, localLightDirection) * localLightDirection.z;
        
        localLightDirection = vec3(0.000000, -0.606266, 0.795262);
        diffuseSum += cloudyLuminance(zenithLuminance, localZenithDirection, localLightDirection) * localLightDirection.z;
        
        localLightDirection = vec3(0.873598, 0.361856, 0.325402);
        diffuseSum += cloudyLuminance(zenithLuminance, localZenithDirection, localLightDirection) * localLightDirection.z;
        
        localLightDirection = vec3(0.361856, 0.873598, 0.325402);
        diffuseSum += cloudyLuminance(zenithLuminance, localZenithDirection, localLightDirection) * localLightDirection.z;
        
        localLightDirection = vec3(-0.361856, 0.873598, 0.325402);
        diffuseSum += cloudyLuminance(zenithLuminance, localZenithDirection, localLightDirection) * localLightDirection.z;
        
        localLightDirection = vec3(-0.873598, 0.361856, 0.325402);
        diffuseSum += cloudyLuminance(zenithLuminance, localZenithDirection, localLightDirection) * localLightDirection.z;
        
        localLightDirection = vec3(-0.873598, -0.361856, 0.325402);
        diffuseSum += cloudyLuminance(zenithLuminance, localZenithDirection, localLightDirection) * localLightDirection.z;
        
        localLightDirection = vec3(-0.361856, -0.873598, 0.325402);
        diffuseSum += cloudyLuminance(zenithLuminance, localZenithDirection, localLightDirection) * localLightDirection.z;
        
        localLightDirection = vec3(0.361856, -0.873598, 0.325402);
        diffuseSum += cloudyLuminance(zenithLuminance, localZenithDirection, localLightDirection) * localLightDirection.z;
        localLightDirection = vec3(0.873598, -0.361856, 0.325402);
        diffuseSum += cloudyLuminance(zenithLuminance, localZenithDirection, localLightDirection) * localLightDirection.z;
        
        return (specularSum + 2.0 * diffuseSum * diffuseBaseColor) / numSamples;
    } else {
        return vec3(0.0, 0.0, 0.0); 
    }
}
"""


litColor =
    """
vec3 litColor(vec4 xyz_type, vec4 rgb_radius, vec3 normalDirection, vec3 directionToCamera, float dotNV, vec3 diffuseBaseColor, vec3 specularBaseColor, float alphaSquared) {
    float lightType = xyz_type.w;
    if (lightType == 0.0) {
        return vec3(0.0, 0.0, 0.0);
    }
    vec3 directionToLight = vec3(0.0, 0.0, 0.0);
    vec3 normalIlluminance = vec3(0.0, 0.0, 0.0);
    getDirectionToLightAndNormalIlluminance(xyz_type, rgb_radius, directionToLight, normalIlluminance);
    float dotNL = positiveDotProduct(normalDirection, directionToLight);
    vec3 specularColor = brdf(normalDirection, directionToCamera, directionToLight, alphaSquared, dotNV, dotNL, specularBaseColor, normalIlluminance);
    return (normalIlluminance * dotNL) * ((diffuseBaseColor / 3.14159265359) + specularColor);
}
"""
