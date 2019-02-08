#version 300 es
precision highp float;

uniform vec3 u_Eye, u_Ref, u_Up;
uniform vec2 u_Dimensions;
uniform float u_Time;
uniform vec3 u_Light;

in vec2 fs_Pos;
out vec4 out_Col;

const float FOV = 45.0;
const float EPSILON = 0.0001;
const float FAR_CLIP = 1000.0;
const int MAX_ITERATIONS = 256;

const vec3 XAXIS = vec3(1, 0, 0);
const vec3 YAXIS = vec3(0, 1, 0);
const vec3 ZAXIS = vec3(0, 0, 1);

const vec2 SEED2 = vec2(0.31415, 0.6456);

struct SdfPoint {
    float distance;
    vec3 normal;
    int material;
    vec3 addedColor;
    float acInfluence;
    vec3 hitRayDir;
    vec3 hitPos;
};

struct BoundingBox {
    vec3 corner;
    vec3 dimensions;
};

vec2 random2(vec2 p, vec2 seed) {
    return fract(sin(vec2(dot(p + seed, vec2(311.7, 127.1)), dot(p + seed, vec2(269.5, 183.3)))) * 85734.3545);
}

float random1( vec2 p , vec2 seed) {
    return fract(sin(dot(p + seed, vec2(127.1, 311.7))) * 43758.5453);
}

float sphereSdf(vec3 sphereCenter, float r, vec3 p) {
    return distance(sphereCenter, p) - r;
}

float boxSdf(vec3 boxCenter, vec3 boxDimensions, vec3 p) {
    vec3 dist = abs(p - boxCenter);
    return length(max(dist - boxDimensions, 0.0)) + min(max(dist.x, max(dist.y, dist.z)), 0.0);
}

float torusSdf(vec3 torusCenter, float majorRadius, float minorRadius, vec3 p) {
    vec3 pos = p - torusCenter;
    return length(vec2(length(pos.xz) - majorRadius, pos.y)) - minorRadius;
}

float capsuleSdf(vec3 capsuleCenter, vec3 start, vec3 end, float radius, vec3 p) {
    vec3 pos = p - capsuleCenter;
    vec3 posStart = pos - start;
    vec3 posLength = end - start;
    float h = clamp(dot(posStart, posLength) / dot(posLength, posLength), 0.0, 1.0);
    return length(posStart - posLength * h ) - radius;
}

float dot2(vec3 v) {
    return dot(v, v);
}

float triangleSdf(vec3 center, vec3 a, vec3 b, vec3 c, vec3 p) {
    vec3 pos = p - center;
    vec3 ba = b - a;
    vec3 pa = pos - a;
    vec3 cb = c - b;
    vec3 pb = pos - b;
    vec3 ac = a - c;
    vec3 pc = pos - c;
    vec3 nor = cross(ba, ac);

    return sqrt(
    (sign(dot(cross(ba, nor), pa)) +
    sign(dot(cross(cb, nor), pb)) +
    sign(dot(cross(ac, nor), pc)) < 2.0)
    ?
    min(min(
    dot2(ba * clamp(dot(ba, pa) / dot2(ba), 0.0, 1.0) - pa),
    dot2(cb * clamp(dot(cb, pb) / dot2(cb), 0.0, 1.0) - pb)),
    dot2(ac * clamp(dot(ac, pc) / dot2(ac), 0.0, 1.0) - pc))
    :
    dot(nor, pa) * dot(nor, pa) / dot2(nor));
}

float cappedConeSdf(vec3 coneCenter, float height, float r1, float r2, vec3 p) {
    vec3 pos = p - coneCenter;
    vec2 q = vec2(length(pos.xz), pos.y);
    
    vec2 k1 = vec2(r2, height);
    vec2 k2 = vec2(r2 - r1, 2.0 * height);
    vec2 ca = vec2(q.x - min(q.x, (q.y < 0.0) ? r1 : r2), abs(q.y) - height);
    vec2 cb = q - k1 + k2 * clamp(dot(k1 - q,k2) / dot(k2, k2), 0.0, 1.0);
    float s = (cb.x < 0.0 && ca.y < 0.0) ? -1.0 : 1.0;
    return s * sqrt(min(dot(ca, ca), dot(cb, cb)));
}

float unionSdf(float d1, float d2, float smoothness) {
    smoothness = smoothness > 0.0 ? max(smoothness, 0.00001) : min(smoothness, 0.00001);
    float t = clamp(0.5 + 0.5 * (-d1 + d2) / smoothness, 0.0, 1.0);
    return mix(d2, d1, t) - smoothness * t * (1.0 - t);
}

float subtractSdf(float d1, float d2, float smoothness) {
    return unionSdf(d1, -d2, -smoothness);
}

float intersectSdf(float d1, float d2, float smoothness) {
    return unionSdf(d1, d2, -smoothness);
}

vec3 rotateSdf(inout vec3 p, float angle, vec3 axis) {
    axis = normalize(axis);
    angle = radians(angle);
    vec4 quat = vec4(axis.xyz * sin(angle / 2.0), cos(angle / 2.0));
    p = p + 2.0 * cross(quat.xyz, cross(quat.xyz, p) + quat.w * p);
    return p;
}

vec3 translateSdf(inout vec3 p, vec3 amount) {
    p = p + amount;
    return p;
}

// Takes an sdfPoint an SDF distance, and a material for the second SDF. Returns the closer SDF
SdfPoint andSdf(SdfPoint sdf1, float sdf2, int material) {
    if (sdf1.distance < sdf2) {
        return sdf1;
    }
    else {
        return SdfPoint(sdf2, vec3(0, 0, 0), material, vec3(0), 0.0, vec3(0), vec3(0));
    }
}

SdfPoint toSdfPoint(float sdf, int material) {
    return SdfPoint(sdf, vec3(0, 0, 0), material, vec3(0), 0.0, vec3(0), vec3(0));
}

float smoothInOut(float t) {
    float a = (cos(2.0 * 3.14159 * (t + 0.5)) + 1.0) * 0.5;
    return a;
}


// This is where the actual scene is constructed
// Each separate sdf should be included with an andSdf
SdfPoint totalSdf(vec3 p) {

    float planetOffset = mod(u_Time * 0.5, 200.0);
    translateSdf(p, vec3(planetOffset, 0, 0));
    float sphere3 = sphereSdf(vec3(100, 0, 7), 2.0, p);
    float ring = intersectSdf(
        torusSdf(vec3(100, 0.5, 7), 3.0, 0.8, p),
        torusSdf(vec3(100, -0.5, 7), 3.0, 0.8, p),
        0.05
    );
    translateSdf(p, vec3(-planetOffset, 0, 0));

    translateSdf(p, vec3(2.0 * cos(u_Time * 0.006), sin(u_Time * 0.01), 0.0));

    float sphere1 = sphereSdf(vec3(1, 0, 0), 1.1, p);
    float box1 = boxSdf(vec3(1.4, 0.5, 0), vec3(0.6, 0.6, 1.0), p);
    float capsule1 = capsuleSdf(vec3(0), vec3(-1, 0, 0), vec3(1, 0, 0), 1.0, p);

    float wings = unionSdf(
        triangleSdf(vec3(0), vec3(0.0, 0, 1), vec3(-0.8, 0, 1), vec3(-0.6, 0, 2.0), p) - 0.3,
        triangleSdf(vec3(0), vec3(0.0, 0, -1), vec3(-0.8, 0, -1), vec3(-0.6, 0, -2.0), p) - 0.3, 0.0
    );

    translateSdf(p, vec3(2, 0, 0));
    rotateSdf(p, 90.0, ZAXIS);
    float cone1 = cappedConeSdf(vec3(0), 0.5, 1.7, 1.2, p);
    translateSdf(p, vec3(0, 0.8, 0));
    float cone2 = cappedConeSdf(vec3(0), 0.5, 1.4, 0.8, p);
    translateSdf(p, vec3(0, -0.3, 0));
    float cone3 = cappedConeSdf(vec3(0), 0.35, 0.7, 0.5, p);
    translateSdf(p, vec3(0, 0.8, 0));
    float cone4 = cappedConeSdf(vec3(0), 0.35, 0.5, 0.3, p);
    translateSdf(p, vec3(0, -1.3, 0));
    rotateSdf(p, -90.0, ZAXIS);

    float bubbleFactor = mod(u_Time * 0.1, 3.0) / 3.0;
    float bubble1 = sphereSdf(vec3(-1.0 - bubbleFactor * 6.0, 0, 0), 1.2 * smoothInOut(bubbleFactor), p);
    bubbleFactor = mod(u_Time * 0.1, 4.0) / 4.0;
    float bubble2 = sphereSdf(vec3(-1.0 - bubbleFactor * 6.0, 0.5, 0.5), smoothInOut(bubbleFactor) * 0.5, p);
    bubbleFactor = mod(u_Time * 0.1, 2.0) / 2.0;
    float bubble3 = sphereSdf(vec3(-1.0 - bubbleFactor * 6.0, -0.3, 0.6), smoothInOut(bubbleFactor) * 0.7, p);
    bubbleFactor = mod(u_Time * 0.1, 3.0) / 3.0;
    float bubble4 = sphereSdf(vec3(-1.0 - bubbleFactor * 6.0, 0.7, -0.6), smoothInOut(bubbleFactor), p);
    float bubble = unionSdf(unionSdf(bubble1, bubble2, 0.7), unionSdf(bubble3, bubble4, 0.7), 0.7);

    translateSdf(p, vec3(-2, 0, 0));
    float sphere2 = sphereSdf(vec3(1, 0, 0), 1.1, p);

    // Body
    SdfPoint res = toSdfPoint(unionSdf(
        unionSdf(subtractSdf(unionSdf(capsule1, sphere1, 0.5), box1, 0.5), wings, 0.2),
        subtractSdf(cone1, cone2, 0.5), 0.5), 2
    );
    // Thrusters
    res = andSdf(res, subtractSdf(cone3, cone4, 0.5), 1);
    // Cockpit
    res = andSdf(res, sphere2, 3);
    // Fire/fuel/whatever it is
    res = andSdf(res, bubble, 4);
    // Planet
    res = andSdf(res, sphere3, 6);
    res = andSdf(res, ring, 5);
    return res;
}

vec3 getNormal(vec3 p) {
    vec3 dx = vec3(EPSILON, 0, 0);
    vec3 dy = vec3(0, EPSILON, 0);
    vec3 dz = vec3(0, 0, EPSILON);

    float gradX = totalSdf(p + dx).distance - totalSdf(p - dx).distance;
    float gradY = totalSdf(p + dy).distance - totalSdf(p - dy).distance;
    float gradZ = totalSdf(p + dz).distance - totalSdf(p - dz).distance;
    return -normalize(vec3(gradX, gradY, gradZ));
}

vec3 cubeProject(vec3 rayDir) {
    float comp = -1.0;
    if (abs(rayDir.x) > abs(rayDir.y)) {
        comp = abs(rayDir.x);
        if (abs(rayDir.x) < abs(rayDir.z)) {
            comp = abs(rayDir.z);
        }
    }
    else {
        comp = abs(rayDir.y);
        if (abs(rayDir.y) < abs(rayDir.z)) {
            comp = abs(rayDir.z);
        }
    }

    return rayDir / comp;
}

vec3 generateBackgroundColor (vec3 rayDir) {
    vec3 cubePos = cubeProject(rayDir);
    vec2 p;

    if (abs(cubePos.x) > 0.9999) {
        p = cubePos.yz * 10.0;
    }
    else if (abs(cubePos.y) > 0.9999) {
        p = cubePos.xz * 10.0;
    }
    else {
        p = cubePos.xy * 10.0;
    }

    vec2 cell = floor(p);
        float closestDistance = 1000.0;
        for (int i = 0; i < 9; i++) {
            vec2 curCell = cell + vec2(i % 3 - 1, floor(float(i / 3) - 1.0));
            vec2 cellPoint = vec2(curCell) + random2(vec2(curCell), SEED2);

            closestDistance = min(closestDistance, distance(cellPoint, p));
        }

        if (closestDistance < 0.03 * random1(p, SEED2) + 0.01 * abs(sin(random1(cell, SEED2) * 2.0 + u_Time * 0.1))) {
            return vec3(1, 1, 0.9);
        }
    return vec3(0.0);
}

SdfPoint raycast(vec3 rayDir) {
    vec3 curPoint = u_Eye;
    SdfPoint curSdf = SdfPoint(FAR_CLIP, vec3(0, 1, 0), -1, vec3(0), 0.0, rayDir, vec3(0));
    bool inMaterial = false;
    vec3 addedColor = vec3(0, 0, 0);
    float acInfluence = 0.0;
    vec3 hitRayDir = rayDir;

    int curIteration = 0;
    while(distance(curPoint, u_Eye) < FAR_CLIP && curIteration < MAX_ITERATIONS) {
        curIteration++;
        curSdf = totalSdf(curPoint);
        if (abs(curSdf.distance) < EPSILON) {
            // Material 3 is reflexive, so we have to keep going even if we hit it
            if (curSdf.material == 3) {
                vec3 normal = getNormal(curPoint);
                vec3 reflect = rayDir - 2.0 * dot(normal, rayDir) * normal;
                rayDir = normalize(reflect);
                curPoint = curPoint + rayDir * EPSILON * 1.5;

                vec3 lightDir = vec3(0, -1, 0);
                vec3 camForward = normalize(u_Ref - u_Eye);
                vec3 halfVec = normalize(lightDir + camForward);
                float shininess = 10.0;
                float spec = pow(max(dot(normal, halfVec), 0.0), shininess);
                float fresnelFactor = clamp(dot(lightDir, normal), 0.0, 1.0);
                addedColor += clamp(vec3(fresnelFactor) + vec3(spec), vec3(0), vec3(1));
                acInfluence = acInfluence + (1.0 - acInfluence) * 0.25;
                hitRayDir = rayDir;

            }
            else {
                curSdf.normal = getNormal(curPoint);
                curSdf.addedColor = addedColor;
                curSdf.acInfluence = acInfluence;
                return curSdf;
            }
        }
        curPoint += rayDir * abs(curSdf.distance);
    }

    curSdf.material = -1; //Background;
    curSdf.addedColor = addedColor;
    curSdf.acInfluence = acInfluence;
    curSdf.hitRayDir = hitRayDir;
    curSdf.hitPos = curPoint;
    return curSdf;
}

float sawTooth(float t) {
    return (t - floor(t));
}

vec3 render(SdfPoint sdf, vec3 rayDir, vec3 camForward) {
    int material = sdf.material;
    vec3 lightDir = u_Light;
    vec3 baseColor = vec3(0.5, 0.2, 0.2);
    vec3 color;

    float ambient = 0.1;
    float lambertianFactor = clamp(dot(lightDir, sdf.normal), ambient, 1.0);
    vec3 lambertColor = clamp(baseColor * lambertianFactor, vec3(0), vec3(1));

    // Lambert
    if (sdf.material == 1) {
        color = lambertColor;
    }
    // Blinn-Phong
    else if (sdf.material == 2) {
        lambertColor = clamp(vec3(0.5) * lambertianFactor, vec3(0), vec3(1));
        vec3 halfVec = normalize(lightDir + camForward);
        float shininess = 10.0;
        float spec = pow(max(dot(sdf.normal, halfVec), 0.0), shininess);
        color = lambertColor + vec3(spec);
    }
    // Glowy Lambert
    else if (sdf.material == 4) {
        color = mix(vec3(1, 1, 0), vec3(1, 0.5, 0), dot(sdf.normal, rayDir));
    }
    // Planet
    else if (sdf.material == 5) {
        color = (mix(
            vec3(0, 0.8, 0.8) + (1.0 - dot(rayDir, sdf.normal)) * vec3(0, 1, 1),
            vec3(0.8, 0, 0.8), dot(sdf.normal, rayDir)
        ) + vec3(sawTooth(cos(rayDir.x * 10.0) + sin(rayDir.y * 10.0))) * 0.2)
        * lambertianFactor;
    }
    else if (sdf.material == 6) {
        color = vec3(1, 0.7, 0.5) * lambertianFactor;
    }
    // Background
    else {
        color = generateBackgroundColor(sdf.hitRayDir);
    }

    vec3 clampedColor = clamp(color, vec3(0), vec3(1));
    return sdf.addedColor * sdf.acInfluence + clampedColor * (1.0 - sdf.acInfluence);
}

void main() {
    vec3 forward = normalize(u_Ref - u_Eye);
    vec3 right = normalize(cross(forward, u_Up));
    float refDist = length(u_Ref - u_Eye);

    float verticalAngle = tan(FOV / 2.0);
    float aspectRatio = u_Dimensions.x / u_Dimensions.y;
    vec3 V = u_Up * refDist * verticalAngle;
    vec3 H = right * refDist * aspectRatio * verticalAngle;
    vec3 worldPoint = u_Ref + H * fs_Pos.x + V * fs_Pos.y;
    vec3 rayDir = normalize(worldPoint - u_Eye);

    vec3 color = render(raycast(rayDir), rayDir, forward);
    out_Col = vec4(color, 1);//vec4(0.5 * (fs_Pos + vec2(1.0)), 0.5 * (sin(u_Time * 3.14159 * 0.01) + 1.0), 1.0);
}
