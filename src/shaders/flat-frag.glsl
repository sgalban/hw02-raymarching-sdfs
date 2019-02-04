#version 300 es
precision highp float;

uniform vec3 u_Eye, u_Ref, u_Up;
uniform vec2 u_Dimensions;
uniform float u_Time;

in vec2 fs_Pos;
out vec4 out_Col;

const float FOV = 45.0;
const float EPSILON = 0.0001;
const float NEAR_CLIP = 0.1;
const float FAR_CLIP = 1000.0;

vec3 generateBackgroundColor (vec3 rayDir) {
    return vec3(0.0);
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

float totalSdf(vec3 p) {
    float offset = (sin(u_Time * 0.03) + 1.0) * 0.0;
    rotateSdf(p, u_Time * 0.8, vec3(0, 0, 1));
    float t1 = torusSdf(vec3(0), 2.0, 0.5, p);
    rotateSdf(p, 2.0 * u_Time * 0.8, vec3(0, 0, 1));
    float t2 = torusSdf(vec3(0), 2.0, 0.5, p);
    return unionSdf(t1, t2, 1.0);
}

vec3 raycast(vec3 rayDir) {
    vec3 curPoint = u_Eye;
    while(distance(curPoint, u_Eye) < FAR_CLIP) {
        float distance = totalSdf(curPoint);
        if (abs(distance) < EPSILON) {
            return (normalize(curPoint) + vec3(1.0)) * 0.5;
        }
        curPoint += rayDir * abs(distance);
    }
    return generateBackgroundColor(rayDir);
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

    vec3 color = raycast(rayDir);
    out_Col = vec4(color, 1);//vec4(0.5 * (fs_Pos + vec2(1.0)), 0.5 * (sin(u_Time * 3.14159 * 0.01) + 1.0), 1.0);
}
